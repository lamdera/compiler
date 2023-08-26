var fs = require('fs');
var package = require('./package.json');
var path = require('path');



// MAIN
//
// This function is used by install.js and by the bin/lamdera backup that gets
// called when --ignore-scripts is enabled.


module.exports = function()
{
	// figure out package of binary
	// turn '0.19.1-1.2.3-alpha' into '0.19.1-1.2.3'
	var version = package.version.replace(/^(\d+\.\d+\.\d+)-(\d+\.\d+\.\d+).*$/, '$1');
	var subPackageName = '@lamdera/compiler-' + process.platform + '-' + process.arch;

	verifyPlatform(version, subPackageName);

	var fileName = process.platform === 'win32' ? 'lamdera.exe' : 'lamdera';

	try
	{
		var subBinaryPath = require.resolve(subPackageName + '/' + fileName);
	}
	catch (error)
	{
		if (error && error.code === 'MODULE_NOT_FOUND')
		{
			exitFailure(version, missingSubPackageHelp(subPackageName));
		}
		else
		{
			exitFailure(version, 'I had trouble requiring the binary package for your platform (' + subPackageName + '):\n\n' + error);
		}
	}

	// Yarn 2 and later ("Berry") always invokes `node` (regardless of configuration)
	// so we cannot do any optimizations there
	var isYarnBerry = /\byarn\/(?!1\.)/.test(process.env.npm_config_user_agent || "");

	// as mentioned in bin/lamdera we cannot do any optimizations on Windows
	if (process.platform === 'win32' || isYarnBerry)
	{
		return subBinaryPath;
	}

	// figure out where to put the binary
	var binaryPath = path.resolve(__dirname, package.bin.lamdera);
	var tmpPath = binaryPath + '.tmp';

	// optimize by replacing the JS bin/lamdera with the native binary directly
	try
	{
		// atomically replace the file with a hard link to the binary
		fs.linkSync(subBinaryPath, tmpPath);
		fs.renameSync(tmpPath, binaryPath);
	}
	catch (error)
	{
		exitFailure(version, 'I had some trouble writing file to disk. It is saying:\n\n' + error);
	}

	return binaryPath;
}



// VERIFY PLATFORM


function verifyPlatform(version, subPackageName)
{
	if (subPackageName in package.optionalDependencies) return;

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



// EXIT FAILURE


function exitFailure(version, message)
{
	console.error(
		'-- ERROR -----------------------------------------------------------------------\n\n'
		+ message
		+ '\n\nNOTE: You can avoid npm entirely by downloading directly from:\n'
		+ 'https://dashboard.lamdera.app/docs/download\n'
		+ 'All this package does is distribute a file from there.\n\n'
		+ '--------------------------------------------------------------------------------\n'
	);
	process.exit(1);
}



// MISSING SUB PACKAGE HELP


function missingSubPackageHelp(subPackageName)
{
	return (
		'I support your platform, but I could not find the binary package (' + subPackageName + ') for it!\n\n'
		+ 'This can happen if you use the "--omit=optional" (or "--no-optional") npm flag.\n'
		+ 'The "optionalDependencies" package.json feature is used by Lamdera to install the correct\n'
		+ 'binary executable for your current platform. Remove that flag to use Lamdera.\n\n'
		+ 'This can also happen if the "node_modules" folder was copied between two operating systems\n'
		+ 'that need different binaries - including "virtual" operating systems like Docker and WSL.\n'
		+ 'If so, try installing with npm rather than copying "node_modules".'
	);
}