# Publishing to Haxelib

The **publish Haxelib** workflow validates and submits the existing release
ZIPs as **rayzor**. Configure the repository's `HAXELIB_PASSWORD` Actions
secret. Successful versioned release builds call this workflow automatically;
nightlies are never submitted.

To publish an existing release, run **publish Haxelib**, set `release_tag`,
and enable `publish`. Leave `publish` unchecked to validate without registry
writes. Validation checks metadata, versions and native libraries or download
checksums. Existing versions are skipped; missing dependencies stop submission.

For the initial publication, ashui's
[publish Haxelib stack](https://github.com/rayzor-blade/ashui/actions/workflows/publish-stack.yml)
workflow publishes `ash-future` and `ash-simd`, then `hlwgpu`, `hlwindow` and
`hlavi`, then ashui's four packages. The stack uses ashui's secret.

The native install macros run during compilation, after Haxelib resolves
Haxe dependencies. They stage host HDLLs (hlavi downloads and caches its
checksummed release binary). Install release ZIPs rather than source archives
when bootstrapping before Haxelib publication.
