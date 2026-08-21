// sys.io.File and sys.FileSystem round trips.
//
// Every one of these reaches the filesystem through Sys.getPath, which calls
// hl_to_utf8 -> hlp_utf16_to_utf8(bytes, 0, NULL). Two bugs there — rejecting
// a null size, and reading len==0 as "empty" rather than "scan to the NUL" —
// made that return null, so every path arrived empty and every call threw
// SysError, no matter how correct the file natives underneath were.
import sys.io.File;
import sys.FileSystem;

class TestFileIO {
    static function main() {
        var dir = "ash_fileio_tmp";
        if (FileSystem.exists(dir)) {
            for (f in FileSystem.readDirectory(dir)) FileSystem.deleteFile(dir + "/" + f);
            FileSystem.deleteDirectory(dir);
        }
        FileSystem.createDirectory(dir);

        var path = dir + "/round.txt";
        File.saveContent(path, "hello world");
        var got = File.getContent(path);

        // Non-ASCII exercises the multi-byte and surrogate paths of the very
        // conversion that was broken.
        var uni = dir + "/uni.txt";
        File.saveContent(uni, "héllo · 世界 · 𝄞");
        var gotUni = File.getContent(uni);

        var out = File.write(dir + "/bin.dat", true);
        for (i in 0...256) out.writeByte(i);
        out.close();
        var inp = File.read(dir + "/bin.dat", true);
        var sum = 0;
        for (_ in 0...256) sum += inp.readByte();
        inp.close();

        var listed = FileSystem.readDirectory(dir);
        // An explicit comparator, not Reflect.compare: passing that as a
        // sort callback segfaults the interpreter, which is a separate
        // pre-existing defect and not what this case is here to cover.
        listed.sort(function(a, b) return a < b ? -1 : (a > b ? 1 : 0));

        var stat = FileSystem.stat(dir + "/bin.dat");

        Sys.println("content=" + got);
        Sys.println("unicode=" + gotUni);
        Sys.println("bytesum=" + sum);
        Sys.println("listed=" + listed.join(","));
        Sys.println("binsize=" + stat.size);
        Sys.println("isdir=" + FileSystem.isDirectory(dir));

        // 11 + 32640 + 3 files + 256 bytes = a single number that moves if any
        // one of these regresses.
        var checksum = got.length + sum + listed.length * 1000 + stat.size;
        Sys.println("Checksum: " + checksum);

        for (f in FileSystem.readDirectory(dir)) FileSystem.deleteFile(dir + "/" + f);
        FileSystem.deleteDirectory(dir);
    }
}
