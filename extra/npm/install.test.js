var zlib = require('zlib');
var path = require('path');
var fs = require('fs');
var { describe, test, expect } = require('bun:test');

var install = require('./install.js');
var binary = require('./binary.js');


// HELPERS

function createTarEntry(name, content)
{
	var buf = Buffer.alloc(512 + Math.ceil(content.length / 512) * 512);
	buf.write(name, 0, 100);                              // file name
	buf.write('0000644\0', 100, 8);                        // mode
	buf.write('0000000\0', 108, 8);                        // uid
	buf.write('0000000\0', 116, 8);                        // gid
	buf.write(content.length.toString(8).padStart(11, '0') + '\0', 124, 12); // size
	buf.write('0'.repeat(11) + '\0', 136, 12);             // mtime
	buf.write('0', 156, 1);                                // type (regular file)
	Buffer.from(content).copy(buf, 512);
	return buf;
}

function createTarGz(entries)
{
	var buffers = entries.map(function(e) { return createTarEntry(e.name, e.content); });
	buffers.push(Buffer.alloc(1024)); // tar EOF (two empty blocks)
	return zlib.gzipSync(Buffer.concat(buffers));
}


// TESTS

describe('extractFileFromTarGzip', function()
{
	test('extracts a file from a tar.gz buffer', function()
	{
		var tgz = createTarGz([
			{ name: 'package/lamdera', content: 'FAKE_BINARY_CONTENT' }
		]);
		var result = install.extractFileFromTarGzip(tgz, 'lamdera');
		expect(result.toString()).toBe('FAKE_BINARY_CONTENT');
	});

	test('extracts the correct file when multiple files exist', function()
	{
		var tgz = createTarGz([
			{ name: 'package/readme.md', content: 'readme stuff' },
			{ name: 'package/lamdera', content: 'THE_BINARY' },
			{ name: 'package/package.json', content: '{}' }
		]);
		var result = install.extractFileFromTarGzip(tgz, 'lamdera');
		expect(result.toString()).toBe('THE_BINARY');
	});

	test('throws when file is not found in archive', function()
	{
		var tgz = createTarGz([
			{ name: 'package/other-file', content: 'something' }
		]);
		expect(function()
		{
			install.extractFileFromTarGzip(tgz, 'lamdera');
		}).toThrow('Could not find');
	});

	test('throws on invalid gzip data', function()
	{
		expect(function()
		{
			install.extractFileFromTarGzip(Buffer.from('not gzip'), 'lamdera');
		}).toThrow('Invalid gzip data');
	});

	test('handles binary content correctly', function()
	{
		var binaryContent = Buffer.alloc(256);
		for (var i = 0; i < 256; i++) binaryContent[i] = i;
		var tgz = createTarGz([
			{ name: 'package/lamdera', content: binaryContent.toString('binary') }
		]);
		var result = install.extractFileFromTarGzip(tgz, 'lamdera');
		expect(result.length).toBe(256);
	});
});


describe('downloadedBinPath', function()
{
	test('constructs path with escaped package name', function()
	{
		var result = binary.downloadedBinPath('@lamdera/compiler-linux-x64', 'lamdera');
		var expected = path.join(__dirname, 'downloaded-@lamdera-compiler-linux-x64-lamdera');
		expect(result).toBe(expected);
	});

	test('handles windows filename', function()
	{
		var result = binary.downloadedBinPath('@lamdera/compiler-win32-x64', 'lamdera.exe');
		expect(result).toContain('downloaded-@lamdera-compiler-win32-x64-lamdera.exe');
	});
});
