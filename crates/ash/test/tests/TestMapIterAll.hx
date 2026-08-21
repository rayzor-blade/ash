// Every Map family crossed with every iterator form, judged by
// order-independent sums so hash-order differences between engines cannot
// fake a failure.
//
// What this pins down (settled against stock HashLink 1.15, which agrees
// with ash's interpreter to the digit): `for (v in map)` over Int-VALUED
// maps mis-unboxes under --mode jit only — the sums come back as heap
// addresses. String-valued maps, keys(), and keyValueIterator().value are
// all correct even under jit, and hybrid with EVERY function force-promoted
// through the same LLVM lowering (--jit-threshold 1) is also correct. The
// defect is therefore module-level state only the whole-module JIT sets up,
// not the shared lowering. BACKLOG carries the discriminator table.
class TestMapIterAll { static function main() {
  // Every map family x every iterator form. Sums are order-independent,
  // so the checksum survives hash-order differences between engines.
  var si = new Map<String,Int>(); si.set("a",1); si.set("b",2); si.set("c",3);
  var ii = new Map<Int,Int>();    ii.set(10,100); ii.set(20,200);
  var is = new Map<Int,String>(); is.set(1,"x"); is.set(2,"yy");
  var ss = new Map<String,String>(); ss.set("k","vvv"); ss.set("m","w");

  var t = 0;
  for (v in si) t += v;                       Sys.println("si values sum=" + t);
  var t2 = 0; for (k in si.keys()) t2 += k.length;  Sys.println("si keys len=" + t2);
  var t3 = 0; for (kv in si.keyValueIterator()) t3 += kv.key.length * kv.value;
  Sys.println("si kv=" + t3);
  var t4 = 0; for (v in ii) t4 += v;          Sys.println("ii values sum=" + t4);
  var t5 = 0; for (k in ii.keys()) t5 += k;   Sys.println("ii keys sum=" + t5);
  var t6 = 0; for (v in is) t6 += v.length;   Sys.println("is values len=" + t6);
  var t7 = 0; for (v in ss) t7 += v.length;   Sys.println("ss values len=" + t7);
  var om = new Map<{},Int>();
  var k1 = {}; var k2 = {};
  om.set(k1, 7); om.set(k2, 8);
  var t8 = 0; for (v in om) t8 += v;          Sys.println("om values sum=" + t8);
  Sys.println("Checksum: " + (t + 10*t2 + 100*t3 + t4 + t5 + t6 + t7 + 1000*t8));
}}
