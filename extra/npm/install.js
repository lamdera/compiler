var fs = require('fs');
var path = require('path');
var zlib = require('zlib');
var https = require('https');
var package = require('./package.json');
var binary = require('./binary.js');



// FETCH
//
// Download a URL, following redirects. Returns a Promise that resolves to a Buffer.


function fetch(url)
{
	return new Promise(function(resolve, reject)
	{
		https.get(url, function(res)
		{
			if ((res.statusCode === 301 || res.statusCode === 302) && res.headers.location)
			{
				return fetch(res.headers.location).then(resolve, reject);
			}
			if (res.statusCode !== 200)
			{
				return reject(new Error('Server responded with ' + res.statusCode));
			}
			var chunks = [];
			res.on('data', function(chunk) { chunks.push(chunk); });
			res.on('end', function() { resolve(Buffer.concat(chunks)); });
		}).on('error', reject);
	});
}



// EXTRACT FILE FROM TAR GZIP
//
// Extract a single file from a .tgz buffer using a minimal tar header parser.
// This avoids needing any external tar dependency.
// npm tarballs contain files prefixed with "package/".


function extractFileFromTarGzip(buffer, subpath)
{
	try
	{
		buffer = zlib.unzipSync(buffer);
	}
	catch (err)
	{
		throw new Error('Invalid gzip data in archive: ' + (err && err.message || err));
	}

	var str = function(i, n)
	{
		return String.fromCharCode.apply(null, buffer.subarray(i, i + n)).replace(/\0.*$/, '');
	};

	var offset = 0;
	subpath = 'package/' + subpath;

	while (offset < buffer.length)
	{
		var name = str(offset, 100);
		var size = parseInt(str(offset + 124, 12), 8);
		offset += 512;
		if (!isNaN(size))
		{
			if (name === subpath)
			{
				return buffer.subarray(offset, offset + size);
			}
			offset += (size + 511) & ~511;
		}
	}

	throw new Error('Could not find ' + JSON.stringify(subpath) + ' in archive');
}



// DOWNLOAD DIRECTLY FROM NPM
//
// As a fallback when the optional dependency was not installed (e.g. due to
// npm's known bug where it prunes cross-platform optional deps from the lockfile),
// download the platform-specific package tarball directly from the npm registry
// and extract the binary from it.


async function downloadDirectlyFromNPM(subPackageName, fileName, binPath)
{
	var scopelessName = subPackageName.replace('@lamdera/', '');
	var url = 'https://registry.npmjs.org/' + subPackageName + '/-/' + scopelessName + '-' + package.version + '.tgz';
	console.error('[lamdera] Trying to download ' + JSON.stringify(url));
	try
	{
		fs.writeFileSync(binPath, extractFileFromTarGzip(await fetch(url), fileName));
		fs.chmodSync(binPath, 0o755);
	}
	catch (e)
	{
		console.error('[lamdera] Failed to download ' + JSON.stringify(url) + ': ' + (e && e.message || e));
		throw e;
	}
}



// MAIN


async function checkAndPreparePackage()
{
	var version = package.version.replace(/^(\d+\.\d+\.\d+)-(\d+\.\d+\.\d+).*$/, '$1');
	var subPackageName = '@lamdera/compiler-' + process.platform + '-' + process.arch;
	var fileName = process.platform === 'win32' ? 'lamdera.exe' : 'lamdera';

	// Verify this platform is supported
	if (!(subPackageName in package.optionalDependencies))
	{
		var situation = process.platform + '-' + process.arch;
		console.error(
			'-- ERROR -----------------------------------------------------------------------\n\n'
			+ 'The lamdera npm package does not support your platform (' + situation + ').\n\n'
			+ 'You can try to manually download an appropriate binary (if there is one) from:\n'
			+ 'https://dashboard.lamdera.app/docs/download\n\n'
			+ 'Or otherwise asking for help on the Lamdera Discord:\n'
			+ 'https://dashboard.lamdera.app/docs/discuss\n\n'
			+ '--------------------------------------------------------------------------------\n'
		);
		process.exit(1);
	}

	try
	{
		require.resolve(subPackageName + '/' + fileName);
	}
	catch (error)
	{
		if (error && error.code === 'MODULE_NOT_FOUND')
		{
			console.error(
				'[lamdera] Failed to find package "' + subPackageName + '" on the file system\n\n'
				+ 'This can happen if you use the "--no-optional" flag, or if npm fails to install\n'
				+ 'the correct platform-specific package (a known npm bug with optional dependencies\n'
				+ 'and lockfiles). This install script will now attempt to download the binary directly\n'
				+ 'from the npm registry as a fallback.\n'
			);

			var binPath = binary.downloadedBinPath(subPackageName, fileName);
			try
			{
				await downloadDirectlyFromNPM(subPackageName, fileName, binPath);
				console.error('[lamdera] Successfully downloaded binary to ' + binPath);
			}
			catch (e)
			{
				console.error(
					'-- ERROR -----------------------------------------------------------------------\n\n'
					+ 'I support your platform, but I could not find the binary package (' + subPackageName + ') for it,\n'
					+ 'and the fallback download from the npm registry also failed.\n\n'
					+ 'This can happen if you use the "--omit=optional" (or "--no-optional") npm flag.\n'
					+ 'The "optionalDependencies" package.json feature is used by Lamdera to install the correct\n'
					+ 'binary executable for your current platform. Remove that flag to use Lamdera.\n\n'
					+ 'This can also happen if the "node_modules" folder was copied between two operating systems\n'
					+ 'that need different binaries - including "virtual" operating systems like Docker and WSL.\n'
					+ 'If so, try installing with npm rather than copying "node_modules".\n\n'
					+ 'NOTE: You can avoid npm entirely by downloading directly from:\n'
					+ 'https://dashboard.lamdera.app/docs/download\n'
					+ 'All this package does is distribute a file from there.\n\n'
					+ '--------------------------------------------------------------------------------\n'
				);
				process.exit(1);
			}
		}
		else
		{
			console.error(
				'-- ERROR -----------------------------------------------------------------------\n\n'
				+ 'I had trouble requiring the binary package for your platform (' + subPackageName + '):\n\n'
				+ error + '\n\n'
				+ 'NOTE: You can avoid npm entirely by downloading directly from:\n'
				+ 'https://dashboard.lamdera.app/docs/download\n'
				+ 'All this package does is distribute a file from there.\n\n'
				+ '--------------------------------------------------------------------------------\n'
			);
			process.exit(1);
		}
	}
}

// Allow testing of internal functions when required as a module,
// but run main when executed directly (node install.js).
if (require.main === module)
{
	checkAndPreparePackage();
}

module.exports = { fetch: fetch, extractFileFromTarGzip: extractFileFromTarGzip, downloadDirectlyFromNPM: downloadDirectlyFromNPM };
