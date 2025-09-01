const fs = require('fs')
const os = require('os')
const path = require('path')

// Read a directory recursively and return its contents as a nested object
// If a file handle is provided, append the content of each file to the given file handle
function readDirRec(dirPath, fileHandle = null) {
    const result = {}
    fs.readdirSync(dirPath).forEach(file => {
        const filePath = path.join(dirPath, file)
        const stats = fs.statSync(filePath)
        const mtime = Math.trunc(stats.mtimeMs)
        if (stats.isDirectory()) {
            result[file] = [readDirRec(filePath, fileHandle), mtime]
        } else {
            result[file] = [stats.size, mtime]
            if (fileHandle) {
                const content = fs.readFileSync(filePath)
                fs.appendFileSync(fileHandle, content)
            }
        }
    })
    return result
}

// Read a directory recursively and return its contents as a nested array/object
// If a file handle is provided, append the content of each file to the given file handle
function readDir(dirPath, fileHandle = null) {
    const stats = fs.statSync(dirPath)
    const mtime = Math.trunc(stats.mtimeMs)
    return [readDirRec(dirPath, fileHandle), mtime]
}

// Export the readDir function for use in other modules
module.exports = { readDir: readDir }

// If this script is run directly, read the directory path provided as an argument
// and write the directory tree and content to stdout
if (require.main === module) {
    const dirPath = process.argv[2]
    if (!dirPath) {
        console.error('Please provide a directory path as the first argument.')
        process.exit(1)
    }
    const tmpDir = fs.mkdtempSync(os.tmpdir() + path.sep)
    try {
        const filePath = path.join(tmpDir, 'content')

        const fileHandle = fs.openSync(filePath, 'w')
        const result = JSON.stringify(readDir(dirPath, fileHandle))
        fs.closeSync(fileHandle)

        const lengthBuffer = Buffer.alloc(4)
        lengthBuffer.writeUInt32BE(result.length, 0)
        process.stdout.write(lengthBuffer)
        process.stdout.write(result)
        process.stdout.write(fs.readFileSync(filePath))
    } catch (error) {
        console.error('Error reading directory:', error.message)
        process.exit(1)
    } finally {
        fs.rmSync(tmpDir, { recursive: true })
    }
}