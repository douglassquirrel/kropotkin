/* MultiFile - A JavaScript library to load multiple files from
   tar archives (see http://gist.github.com/407595)

Copyright (c) 2010 Ilmari Heikkinen
Modified for Kropotkin in 2013 by D Squirrel

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

function parseTar(data) {
    files = [];
    offset = 0;
    while (data.length >= offset + 512) {
        var header = files.length == 0 ? null : files[files.length-1];
        if (header && header.data == null) {
            if (offset + header.length <= data.length) {
                header.data = data.substring(offset, offset+header.length);
                offset += 512 * Math.ceil(header.length / 512);
            } else { // not loaded yet
                break;
            }
        } else {
            var header = this.parseTarHeader(data, offset);
            if (header.length > 0 || header.filename != '') {
                files.push(header);
                offset += 512;
                header.offset = offset;
            } else { // empty header, stop processing
                offset = data.length;
            }
        }
    }
    return files;
}

function parseTarHeader(data, offset) {
    var i = offset || 0;
    var h = {};
    h.filename = data.substring(i, i+=100).split("\0", 1)[0];
    h.mode = data.substring(i, i+=8).split("\0", 1)[0];
    h.uid = data.substring(i, i+=8).split("\0", 1)[0];
    h.gid = data.substring(i, i+=8).split("\0", 1)[0];
    h.length = parseTarNumber(data.substring(i, i+=12));
    h.lastModified = data.substring(i, i+=12).split("\0", 1)[0];
    h.checkSum = data.substring(i, i+=8).split("\0", 1)[0];
    h.fileType = data.substring(i, i+=1).split("\0", 1)[0];
    h.linkName = data.substring(i, i+=100).split("\0", 1)[0];
    return h;
}

function parseTarNumber(data) {
    // if (data.charCodeAt(0) & 0x80 == 1) {
    // GNU tar 8-byte binary big-endian number
    // } else {
    return parseInt('0'+data.replace(/[^\d]/g, ''));
    // }
}

report_deployment('tarfile.js');