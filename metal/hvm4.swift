// HVM4 Metal Host Code
// =====================
//
// This file provides the host-side Swift code for the HVM4 Metal runtime.
// It handles parsing, Metal setup, kernel dispatch, and result display.

import Foundation
import Metal

// Types
// =====

typealias Term = UInt64

// Tags
// ====

let NAM: UInt8  = 0
let DRY: UInt8  = 1
let REF: UInt8  = 2
let ALO: UInt8  = 3
let ERA: UInt8  = 4
let CO0: UInt8  = 5
let CO1: UInt8  = 6
let VAR: UInt8  = 7
let LAM: UInt8  = 8
let APP: UInt8  = 9
let SUP: UInt8  = 10
let DUP: UInt8  = 11
let MAT: UInt8  = 12
let CTR: UInt8  = 13
let CTR_MAX_ARI: UInt8 = 16

// Bit Layout
// ==========

let SUB_SHIFT: UInt64 = 63
let TAG_SHIFT: UInt64 = 56
let EXT_SHIFT: UInt64 = 32

let TAG_MASK: UInt64 = 0x7F
let EXT_MASK: UInt64 = 0xFFFFFF
let VAL_MASK: UInt64 = 0xFFFFFFFF

// Capacities
// ==========

let BOOK_CAP: Int  = 1 << 24   // 16M book entries (name -> loc)
let HEAP_CAP: Int  = 1 << 26   // 64M heap terms (512MB) for single-thread
let STACK_CAP: Int = 1 << 21   // 2M stack entries (16MB) for single-thread

// Multi-threaded capacities
let MT_HEAP_PER_THR: UInt64  = 1 << 21   // 2M terms per thread (16MB) - benchmark uses ~1M
let MT_STACK_PER_THR: UInt64 = 1 << 19   // 512K entries per thread (4MB)

// Globals for parsing
// ===================

var HEAP: UnsafeMutablePointer<Term>!
var BOOK: UnsafeMutablePointer<UInt32>!
var ALLOC: UInt64 = 1

// Term Helpers
// ============

@inline(__always)
func newTerm(_ sub: UInt8, _ tag: UInt8, _ ext: UInt32, _ val: UInt32) -> Term {
  return (UInt64(sub) << SUB_SHIFT)
       | (UInt64(tag & UInt8(TAG_MASK)) << TAG_SHIFT)
       | (UInt64(ext & UInt32(EXT_MASK)) << EXT_SHIFT)
       | UInt64(val & UInt32(VAL_MASK))
}

@inline(__always)
func tag(_ t: Term) -> UInt8 {
  return UInt8((t >> TAG_SHIFT) & TAG_MASK)
}

@inline(__always)
func ext(_ t: Term) -> UInt32 {
  return UInt32((t >> EXT_SHIFT) & EXT_MASK)
}

@inline(__always)
func val(_ t: Term) -> UInt32 {
  return UInt32(t & VAL_MASK)
}

@inline(__always)
func arityOf(_ t: Term) -> UInt32 {
  let tg = tag(t)
  if tg == LAM { return 1 }
  if tg == APP || tg == SUP || tg == DUP || tg == MAT || tg == DRY { return 2 }
  if tg >= CTR && tg <= CTR + CTR_MAX_ARI { return UInt32(tg - CTR) }
  return 0
}

@inline(__always)
func heapAlloc(_ size: UInt64) -> UInt64 {
  let at = ALLOC
  ALLOC += size
  return at
}

// Names
// =====

let alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"

func charToB64(_ c: Character) -> Int {
  if c == "_" { return 0 }
  if c >= "a" && c <= "z" { return 1 + Int(c.asciiValue! - Character("a").asciiValue!) }
  if c >= "A" && c <= "Z" { return 27 + Int(c.asciiValue! - Character("A").asciiValue!) }
  if c >= "0" && c <= "9" { return 53 + Int(c.asciiValue! - Character("0").asciiValue!) }
  if c == "$" { return 63 }
  return -1
}

func isNameStart(_ c: Character) -> Bool {
  return (c >= "a" && c <= "z") || (c >= "A" && c <= "Z")
}

func isNameChar(_ c: Character) -> Bool {
  return charToB64(c) >= 0
}

func nameToStr(_ n: UInt32) -> String {
  if n == 0 { return "_" }
  var result = ""
  var val = n
  var chars: [Character] = []
  while val > 0 {
    let idx = Int(val % 64)
    chars.append(alphabet[alphabet.index(alphabet.startIndex, offsetBy: idx)])
    val /= 64
  }
  for c in chars.reversed() {
    result.append(c)
  }
  return result
}

// Term Constructors (for parsing)
// ===============================

func Var(_ loc: UInt32) -> Term { newTerm(0, VAR, 0, loc) }
func Ref(_ nam: UInt32) -> Term { newTerm(0, REF, nam, 0) }
func Era() -> Term { newTerm(0, ERA, 0, 0) }

func Lam(_ bod: Term) -> Term {
  let loc = UInt32(heapAlloc(1))
  HEAP[Int(loc)] = bod
  return newTerm(0, LAM, 0, loc)
}

func App(_ fun: Term, _ arg: Term) -> Term {
  let loc = UInt32(heapAlloc(2))
  HEAP[Int(loc) + 0] = fun
  HEAP[Int(loc) + 1] = arg
  return newTerm(0, APP, 0, loc)
}

func Sup(_ lab: UInt32, _ tm0: Term, _ tm1: Term) -> Term {
  let loc = UInt32(heapAlloc(2))
  HEAP[Int(loc) + 0] = tm0
  HEAP[Int(loc) + 1] = tm1
  return newTerm(0, SUP, lab, loc)
}

func Dup(_ lab: UInt32, _ v: Term, _ bod: Term) -> Term {
  let loc = UInt32(heapAlloc(2))
  HEAP[Int(loc) + 0] = v
  HEAP[Int(loc) + 1] = bod
  return newTerm(0, DUP, lab, loc)
}

func Mat(_ nam: UInt32, _ v: Term, _ nxt: Term) -> Term {
  let loc = UInt32(heapAlloc(2))
  HEAP[Int(loc) + 0] = v
  HEAP[Int(loc) + 1] = nxt
  return newTerm(0, MAT, nam, loc)
}

func Ctr(_ nam: UInt32, _ args: [Term]) -> Term {
  let loc = UInt32(heapAlloc(UInt64(args.count)))
  for i in 0..<args.count {
    HEAP[Int(loc) + i] = args[i]
  }
  return newTerm(0, CTR + UInt8(args.count), nam, loc)
}

// Parser
// ======

struct PState {
  var file: String
  var src: String
  var pos: Int
  var line: Int
  var col: Int
}

struct PBind {
  var name: UInt32
  var depth: UInt32
  var lab: UInt32
}

var PARSE_BINDS: [PBind] = []

func parseError(_ s: PState, _ expected: String, _ detected: Character?) {
  fputs("\u{001B}[1;31mPARSE_ERROR\u{001B}[0m (\(s.file):\(s.line):\(s.col))\n", stderr)
  fputs("- expected: \(expected)\n", stderr)
  if let c = detected {
    fputs("- detected: '\(c)'\n", stderr)
  } else {
    fputs("- detected: EOF\n", stderr)
  }
  exit(1)
}

func atEnd(_ s: PState) -> Bool { s.pos >= s.src.count }

func peek(_ s: PState) -> Character? {
  if atEnd(s) { return nil }
  return s.src[s.src.index(s.src.startIndex, offsetBy: s.pos)]
}

func peekAt(_ s: PState, _ offset: Int) -> Character? {
  let idx = s.pos + offset
  if idx >= s.src.count { return nil }
  return s.src[s.src.index(s.src.startIndex, offsetBy: idx)]
}

func advance(_ s: inout PState) {
  if atEnd(s) { return }
  if peek(s) == "\n" { s.line += 1; s.col = 1 } else { s.col += 1 }
  s.pos += 1
}

func startsWith(_ s: PState, _ str: String) -> Bool {
  for (i, c) in str.enumerated() {
    if peekAt(s, i) != c { return false }
  }
  return true
}

func match(_ s: inout PState, _ str: String) -> Bool {
  if !startsWith(s, str) { return false }
  for _ in str { advance(&s) }
  return true
}

func isSpace(_ c: Character) -> Bool {
  c == " " || c == "\t" || c == "\n" || c == "\r"
}

func skip(_ s: inout PState) {
  while !atEnd(s) {
    if let c = peek(s), isSpace(c) { advance(&s); continue }
    if startsWith(s, "//") {
      while !atEnd(s) && peek(s) != "\n" { advance(&s) }
      continue
    }
    break
  }
}

func consume(_ s: inout PState, _ str: String) {
  skip(&s)
  if !match(&s, str) { parseError(s, str, peek(s)) }
  skip(&s)
}

func bindPush(_ name: UInt32, _ depth: UInt32, _ lab: UInt32) {
  PARSE_BINDS.append(PBind(name: name, depth: depth, lab: lab))
}

func bindPop() { PARSE_BINDS.removeLast() }

func bindLookup(_ name: UInt32, _ depth: UInt32) -> (idx: Int, lab: UInt32) {
  for i in stride(from: PARSE_BINDS.count - 1, through: 0, by: -1) {
    if PARSE_BINDS[i].name == name {
      return (Int(depth) - 1 - Int(PARSE_BINDS[i].depth), PARSE_BINDS[i].lab)
    }
  }
  return (-1, 0)
}

func parseName(_ s: inout PState) -> UInt32 {
  skip(&s)
  guard let c = peek(s), isNameStart(c) else { parseError(s, "name", peek(s)); return 0 }
  var k: UInt32 = 0
  while let c = peek(s), isNameChar(c) {
    k = ((k << 6) + UInt32(charToB64(c))) & UInt32(EXT_MASK)
    advance(&s)
  }
  skip(&s)
  return k
}

func parseMatBody(_ s: inout PState, _ depth: UInt32) -> Term {
  skip(&s)
  if peek(s) == "}" { consume(&s, "}"); return Era() }
  if peek(s) == "#" {
    consume(&s, "#")
    let nam = parseName(&s)
    consume(&s, ":")
    let v = parseTerm(&s, depth)
    skip(&s); _ = match(&s, ";"); skip(&s)
    let nxt = parseMatBody(&s, depth)
    return Mat(nam, v, nxt)
  }
  let v = parseTerm(&s, depth)
  consume(&s, "}")
  return v
}

func parseLam(_ s: inout PState, _ depth: UInt32) -> Term {
  skip(&s)
  if peek(s) == "{" { consume(&s, "{"); return parseMatBody(&s, depth) }
  let nam = parseName(&s)
  consume(&s, ".")
  bindPush(nam, depth, 0)
  let loc = UInt32(heapAlloc(1))
  let body = parseTerm(&s, depth + 1)
  HEAP[Int(loc)] = body
  bindPop()
  return newTerm(0, LAM, depth, loc)
}

func parseDup(_ s: inout PState, _ depth: UInt32) -> Term {
  let nam = parseName(&s)
  consume(&s, "&")
  let lab = parseName(&s)
  consume(&s, "=")
  let v = parseTerm(&s, depth)
  skip(&s); _ = match(&s, ";"); skip(&s)
  bindPush(nam, depth, lab)
  let loc = UInt32(heapAlloc(2))
  HEAP[Int(loc)] = v
  let body = parseTerm(&s, depth + 1)
  HEAP[Int(loc) + 1] = body
  bindPop()
  return newTerm(0, DUP, lab, loc)
}

func parseSup(_ s: inout PState, _ depth: UInt32) -> Term {
  skip(&s)
  if peek(s) == "{" { consume(&s, "{"); consume(&s, "}"); return Era() }
  let lab = parseName(&s)
  consume(&s, "{")
  let tm0 = parseTerm(&s, depth)
  skip(&s); _ = match(&s, ","); skip(&s)
  let tm1 = parseTerm(&s, depth)
  consume(&s, "}")
  return Sup(lab, tm0, tm1)
}

func parseCtr(_ s: inout PState, _ depth: UInt32) -> Term {
  let nam = parseName(&s)
  consume(&s, "{")
  var args: [Term] = []
  skip(&s)
  if peek(s) != "}" {
    while true {
      args.append(parseTerm(&s, depth))
      skip(&s)
      if peek(s) == "," { consume(&s, ","); continue }
      break
    }
  }
  consume(&s, "}")
  return Ctr(nam, args)
}

func parseRef(_ s: inout PState) -> Term { Ref(parseName(&s)) }

func parsePar(_ s: inout PState, _ depth: UInt32) -> Term {
  let term = parseTerm(&s, depth)
  consume(&s, ")")
  return term
}

func parseVar(_ s: inout PState, _ depth: UInt32) -> Term {
  skip(&s)
  let nam = parseName(&s)
  let (idx, lab) = bindLookup(nam, depth)
  skip(&s)
  var side = -1
  if match(&s, "₀") { side = 0 }
  else if match(&s, "₁") { side = 1 }
  skip(&s)
  let v = idx >= 0 ? UInt32(idx) : nam
  let tg: UInt8 = side == 0 ? CO0 : (side == 1 ? CO1 : VAR)
  return newTerm(0, tg, lab, v)
}

func parseApp(_ f: Term, _ s: inout PState, _ depth: UInt32) -> Term {
  skip(&s)
  if peek(s) != "(" { return f }
  var fun = f
  consume(&s, "(")
  if peek(s) == ")" { consume(&s, ")"); return parseApp(fun, &s, depth) }
  while true {
    let arg = parseTerm(&s, depth)
    fun = App(fun, arg)
    skip(&s)
    if peek(s) == "," { consume(&s, ","); continue }
    if peek(s) == ")" { consume(&s, ")"); break }
    parseError(s, "',' or ')'", peek(s))
  }
  return parseApp(fun, &s, depth)
}

func parseTerm(_ s: inout PState, _ depth: UInt32) -> Term {
  skip(&s)
  var t: Term
  if match(&s, "λ") { t = parseLam(&s, depth) }
  else if match(&s, "!") { t = parseDup(&s, depth) }
  else if match(&s, "&") { t = parseSup(&s, depth) }
  else if match(&s, "#") { t = parseCtr(&s, depth) }
  else if match(&s, "@") { t = parseRef(&s) }
  else if match(&s, "(") { t = parsePar(&s, depth) }
  else { t = parseVar(&s, depth) }
  return parseApp(t, &s, depth)
}

func parseDef(_ s: inout PState) {
  skip(&s)
  if atEnd(s) { return }
  if match(&s, "@") {
    let nam = parseName(&s) & UInt32(EXT_MASK)
    consume(&s, "=")
    PARSE_BINDS = []
    let v = parseTerm(&s, 0)
    let loc = UInt32(heapAlloc(1))
    HEAP[Int(loc)] = v
    BOOK[Int(nam)] = loc
    parseDef(&s)
    return
  }
  parseError(s, "definition", peek(s))
}

// Result Stringifier
// ==================

func strTerm(_ heap: UnsafePointer<Term>, _ term: Term, _ depth: UInt32 = 0) -> String {
  switch tag(term) {
    case VAR, NAM:
      return nameToStr(val(term))
    case REF:
      return "@\(nameToStr(ext(term)))"
    case ERA:
      return "&{}"
    case CO0:
      return "\(nameToStr(val(term)))₀"
    case CO1:
      return "\(nameToStr(val(term)))₁"
    case LAM:
      let loc = val(term)
      let nam = depth + 1
      return "λ\(nameToStr(nam)).\(strTerm(heap, heap[Int(loc)], depth + 1))"
    case APP, DRY:
      var spine: [Term] = []
      var curr = term
      while (tag(curr) == APP || tag(curr) == DRY) && spine.count < 256 {
        let loc = val(curr)
        spine.append(heap[Int(loc) + 1])
        curr = heap[Int(loc)]
      }
      var result = tag(curr) == LAM ? "(\(strTerm(heap, curr, depth)))" : strTerm(heap, curr, depth)
      result += "("
      for i in 0..<spine.count {
        if i > 0 { result += "," }
        result += strTerm(heap, spine[spine.count - 1 - i], depth)
      }
      result += ")"
      return result
    case SUP:
      let loc = val(term)
      return "&\(nameToStr(ext(term))){\(strTerm(heap, heap[Int(loc)], depth)),\(strTerm(heap, heap[Int(loc) + 1], depth))}"
    case DUP:
      let loc = val(term)
      let nam = depth + 1
      return "!\(nameToStr(nam))&\(nameToStr(ext(term)))=\(strTerm(heap, heap[Int(loc)], depth));\(strTerm(heap, heap[Int(loc) + 1], depth + 1))"
    case MAT:
      let loc = val(term)
      return "λ{#\(nameToStr(ext(term))):\(strTerm(heap, heap[Int(loc)], depth));\(strTerm(heap, heap[Int(loc) + 1], depth))}"
    case ALO:
      return "<ALO>"
    default:
      if tag(term) >= CTR && tag(term) <= CTR + CTR_MAX_ARI {
        let ari = Int(tag(term) - CTR)
        let loc = val(term)
        var result = "#\(nameToStr(ext(term))){"
        for i in 0..<ari {
          if i > 0 { result += "," }
          result += strTerm(heap, heap[Int(loc) + i], depth)
        }
        result += "}"
        return result
      }
      return "<?>"
  }
}

// Main
// ====

func main() {
  // Get Metal device
  guard let device = MTLCreateSystemDefaultDevice() else {
    fputs("Error: No Metal device found\n", stderr)
    exit(1)
  }

  // Parse command line arguments
  var numThreads: Int = 1
  var srcPath: String? = nil

  var i = 1
  while i < CommandLine.arguments.count {
    let arg = CommandLine.arguments[i]
    if arg.hasPrefix("-t") {
      if let n = Int(arg.dropFirst(2)), n > 0 {
        numThreads = n
      }
    } else if arg == "--bench" {
      // Run benchmark with 1, 2, 4, 8, ..., 1024 threads
      runBenchmarkSuite(device)
      return
    } else {
      srcPath = arg
    }
    i += 1
  }

  // Allocate host memory for parsing
  HEAP = UnsafeMutablePointer<Term>.allocate(capacity: HEAP_CAP)
  HEAP.initialize(repeating: 0, count: HEAP_CAP)
  BOOK = UnsafeMutablePointer<UInt32>.allocate(capacity: BOOK_CAP)
  BOOK.initialize(repeating: 0, count: BOOK_CAP)

  // Default benchmark
  let testSrc = """
    @ctru = λt.λf.t
    @cfal = λt.λf.f
    @cnot = λx.x(@cfal,@ctru)
    @P24  = λf.
      ! F &A = f;
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      λk. F₀(F₁(k))
    @main = @P24(@cnot,@ctru)
    """

  var src = testSrc
  if let path = srcPath {
    if let contents = try? String(contentsOfFile: path, encoding: .utf8) {
      src = contents
    } else {
      fputs("Error: could not open '\(path)'\n", stderr)
      exit(1)
    }
  }

  // Parse
  var s = PState(
    file: srcPath ?? "<inline>",
    src: src,
    pos: 0,
    line: 1,
    col: 1
  )
  parseDef(&s)

  let bookSize = UInt32(ALLOC)

  // Get @main name hash
  var mainName: UInt32 = 0
  for c in "main" {
    mainName = ((mainName << 6) + UInt32(charToB64(c))) & UInt32(EXT_MASK)
  }

  guard BOOK[Int(mainName)] != 0 else {
    fputs("Error: @main not found\n", stderr)
    exit(1)
  }

  // Load Metal shader
  guard let shaderSource = loadShaderSource() else {
    fputs("Error: Could not load hvm4.metal shader\n", stderr)
    exit(1)
  }

  if numThreads == 1 {
    runSingleThreaded(device, shaderSource, bookSize, mainName)
  } else {
    runMultiThreaded(device, shaderSource, bookSize, mainName, numThreads)
  }
}

func loadShaderSource() -> String? {
  let metalPath = URL(fileURLWithPath: CommandLine.arguments[0])
    .deletingLastPathComponent()
    .appendingPathComponent("hvm4.metal")

  if let source = try? String(contentsOf: metalPath, encoding: .utf8) {
    return source
  }
  return try? String(contentsOfFile: "hvm4.metal", encoding: .utf8)
}

func runSingleThreaded(_ device: MTLDevice, _ shaderSource: String, _ bookSize: UInt32, _ mainName: UInt32) {
  // Compile shader
  let library: MTLLibrary
  do {
    library = try device.makeLibrary(source: shaderSource, options: nil)
  } catch {
    fputs("Error compiling shader: \(error)\n", stderr)
    exit(1)
  }

  guard let function = library.makeFunction(name: "hvm_run") else {
    fputs("Error: Could not find hvm_run kernel\n", stderr)
    exit(1)
  }

  let pipeline: MTLComputePipelineState
  do {
    pipeline = try device.makeComputePipelineState(function: function)
  } catch {
    fputs("Error creating pipeline: \(error)\n", stderr)
    exit(1)
  }

  guard let queue = device.makeCommandQueue() else {
    fputs("Error: Could not create command queue\n", stderr)
    exit(1)
  }

  // Create Metal buffers
  let heapBuffer = device.makeBuffer(
    bytes: HEAP,
    length: HEAP_CAP * MemoryLayout<Term>.stride,
    options: .storageModeShared
  )!

  let stackBuffer = device.makeBuffer(
    length: STACK_CAP * MemoryLayout<Term>.stride,
    options: .storageModeShared
  )!

  let bookBuffer = device.makeBuffer(
    bytes: BOOK,
    length: BOOK_CAP * MemoryLayout<UInt32>.stride,
    options: .storageModeShared
  )!

  // Result buffer: [alloc, itrs_lo, itrs_hi, result_tag, result_val]
  let resultBuffer = device.makeBuffer(
    length: 5 * MemoryLayout<UInt32>.stride,
    options: .storageModeShared
  )!

  var initAlloc = bookSize
  let initAllocBuffer = device.makeBuffer(
    bytes: &initAlloc,
    length: MemoryLayout<UInt32>.stride,
    options: .storageModeShared
  )!

  var mainRef = mainName
  let mainRefBuffer = device.makeBuffer(
    bytes: &mainRef,
    length: MemoryLayout<UInt32>.stride,
    options: .storageModeShared
  )!

  // Warm up runs
  for _ in 0..<3 {
    // Reset heap from book data
    memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)

    guard let cmdBuffer = queue.makeCommandBuffer(),
          let encoder = cmdBuffer.makeComputeCommandEncoder() else {
      fputs("Error: Could not create command encoder\n", stderr)
      exit(1)
    }

    encoder.setComputePipelineState(pipeline)
    encoder.setBuffer(heapBuffer, offset: 0, index: 0)
    encoder.setBuffer(stackBuffer, offset: 0, index: 1)
    encoder.setBuffer(bookBuffer, offset: 0, index: 2)
    encoder.setBuffer(resultBuffer, offset: 0, index: 3)
    encoder.setBuffer(initAllocBuffer, offset: 0, index: 4)
    encoder.setBuffer(mainRefBuffer, offset: 0, index: 5)

    // 1 thread, 1 threadgroup
    encoder.dispatchThreads(MTLSize(width: 1, height: 1, depth: 1),
                            threadsPerThreadgroup: MTLSize(width: 1, height: 1, depth: 1))
    encoder.endEncoding()
    cmdBuffer.commit()
    cmdBuffer.waitUntilCompleted()
  }

  // Benchmark run
  memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)

  guard let cmdBuffer = queue.makeCommandBuffer(),
        let encoder = cmdBuffer.makeComputeCommandEncoder() else {
    fputs("Error: Could not create command encoder\n", stderr)
    exit(1)
  }

  encoder.setComputePipelineState(pipeline)
  encoder.setBuffer(heapBuffer, offset: 0, index: 0)
  encoder.setBuffer(stackBuffer, offset: 0, index: 1)
  encoder.setBuffer(bookBuffer, offset: 0, index: 2)
  encoder.setBuffer(resultBuffer, offset: 0, index: 3)
  encoder.setBuffer(initAllocBuffer, offset: 0, index: 4)
  encoder.setBuffer(mainRefBuffer, offset: 0, index: 5)

  encoder.dispatchThreads(MTLSize(width: 1, height: 1, depth: 1),
                          threadsPerThreadgroup: MTLSize(width: 1, height: 1, depth: 1))
  encoder.endEncoding()

  let start = DispatchTime.now()
  cmdBuffer.commit()
  cmdBuffer.waitUntilCompleted()
  let end = DispatchTime.now()

  let wallTime = Double(end.uptimeNanoseconds - start.uptimeNanoseconds) / 1_000_000_000.0

  // Read results
  let resultPtr = resultBuffer.contents().bindMemory(to: UInt32.self, capacity: 5)
  let finalAlloc = resultPtr[0]
  let itrsLo = UInt64(resultPtr[1])
  let itrsHi = UInt64(resultPtr[2])
  let totalItrs = (itrsHi << 32) | itrsLo

  let mips = Double(totalItrs) / wallTime / 1_000_000.0

  // Read final result for display
  let resultTag = UInt8(resultPtr[3])
  let resultVal = resultPtr[4]
  let resultTerm = newTerm(0, resultTag, 0, resultVal)

  // Get heap for stringification
  let heapPtr = heapBuffer.contents().bindMemory(to: Term.self, capacity: HEAP_CAP)

  print("Threads: 1")
  print("Result: \(strTerm(heapPtr, resultTerm))")
  print("Interactions: \(totalItrs)")
  print("Time: \(String(format: "%.6f", wallTime))s")
  print("Performance: \(String(format: "%.2f", mips)) MIPS")
  print("Heap used: \(finalAlloc) terms")
}

func runMultiThreaded(_ device: MTLDevice, _ shaderSource: String, _ bookSize: UInt32, _ mainName: UInt32, _ numThreads: Int) {
  // Compile shader
  let library: MTLLibrary
  do {
    library = try device.makeLibrary(source: shaderSource, options: nil)
  } catch {
    fputs("Error compiling shader: \(error)\n", stderr)
    exit(1)
  }

  guard let function = library.makeFunction(name: "hvm_run_mt") else {
    fputs("Error: Could not find hvm_run_mt kernel\n", stderr)
    exit(1)
  }

  let pipeline: MTLComputePipelineState
  do {
    pipeline = try device.makeComputePipelineState(function: function)
  } catch {
    fputs("Error creating pipeline: \(error)\n", stderr)
    exit(1)
  }

  guard let queue = device.makeCommandQueue() else {
    fputs("Error: Could not create command queue\n", stderr)
    exit(1)
  }

  // Memory layout (CUDA-style warp-coalesced):
  //   heap: [book region | warp0 slice | warp1 slice | ...]
  //   Each warp slice = heap_per_thr * simd_width terms, with interleaved access
  let simdWidth = pipeline.threadExecutionWidth
  let heapPerThr = MT_HEAP_PER_THR
  let stackPerThr = MT_STACK_PER_THR

  let numWarps = (numThreads + simdWidth - 1) / simdWidth
  let warpSliceSize = Int(heapPerThr) * simdWidth
  let totalHeapSize = Int(bookSize) + numWarps * warpSliceSize
  let totalStackSize = numThreads * Int(stackPerThr)

  // Create buffers
  // Heap: copy book data at start
  let heapBuffer = device.makeBuffer(length: totalHeapSize * MemoryLayout<Term>.stride, options: .storageModeShared)!
  memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)

  let stackBuffer = device.makeBuffer(length: totalStackSize * MemoryLayout<Term>.stride, options: .storageModeShared)!

  let bookBuffer = device.makeBuffer(bytes: BOOK, length: BOOK_CAP * MemoryLayout<UInt32>.stride, options: .storageModeShared)!

  // Per-thread output buffers
  let itrsBuffer = device.makeBuffer(length: numThreads * MemoryLayout<UInt64>.stride, options: .storageModeShared)!
  let outputsBuffer = device.makeBuffer(length: numThreads * MemoryLayout<Term>.stride, options: .storageModeShared)!

  // Constant buffers
  var bookSizeVal = bookSize
  let bookSizeBuffer = device.makeBuffer(bytes: &bookSizeVal, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!

  var mainRef = mainName
  let mainRefBuffer = device.makeBuffer(bytes: &mainRef, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!

  var numThreadsVal = UInt32(numThreads)
  let numThreadsBuffer = device.makeBuffer(bytes: &numThreadsVal, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!

  var heapPerThrVal = heapPerThr
  let heapPerThrBuffer = device.makeBuffer(bytes: &heapPerThrVal, length: MemoryLayout<UInt64>.stride, options: .storageModeShared)!

  var stackPerThrVal = stackPerThr
  let stackPerThrBuffer = device.makeBuffer(bytes: &stackPerThrVal, length: MemoryLayout<UInt64>.stride, options: .storageModeShared)!

  var simdWidthVal = UInt32(simdWidth)
  let simdWidthBuffer = device.makeBuffer(bytes: &simdWidthVal, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!

  // Warm up
  for _ in 0..<3 {
    memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)

    guard let cmdBuffer = queue.makeCommandBuffer(),
          let encoder = cmdBuffer.makeComputeCommandEncoder() else {
      fputs("Error: Could not create command encoder\n", stderr)
      exit(1)
    }

    encoder.setComputePipelineState(pipeline)
    encoder.setBuffer(heapBuffer, offset: 0, index: 0)
    encoder.setBuffer(stackBuffer, offset: 0, index: 1)
    encoder.setBuffer(bookBuffer, offset: 0, index: 2)
    encoder.setBuffer(itrsBuffer, offset: 0, index: 3)
    encoder.setBuffer(outputsBuffer, offset: 0, index: 4)
    encoder.setBuffer(bookSizeBuffer, offset: 0, index: 5)
    encoder.setBuffer(mainRefBuffer, offset: 0, index: 6)
    encoder.setBuffer(numThreadsBuffer, offset: 0, index: 7)
    encoder.setBuffer(heapPerThrBuffer, offset: 0, index: 8)
    encoder.setBuffer(stackPerThrBuffer, offset: 0, index: 9)
    encoder.setBuffer(simdWidthBuffer, offset: 0, index: 10)

    // Use single threadgroup for now
    let threadsPerGroup = min(numThreads, pipeline.maxTotalThreadsPerThreadgroup)
    encoder.dispatchThreads(MTLSize(width: numThreads, height: 1, depth: 1),
                            threadsPerThreadgroup: MTLSize(width: threadsPerGroup, height: 1, depth: 1))
    encoder.endEncoding()
    cmdBuffer.commit()
    cmdBuffer.waitUntilCompleted()
  }

  // Benchmark run
  memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)

  guard let cmdBuffer = queue.makeCommandBuffer(),
        let encoder = cmdBuffer.makeComputeCommandEncoder() else {
    fputs("Error: Could not create command encoder\n", stderr)
    exit(1)
  }

  encoder.setComputePipelineState(pipeline)
  encoder.setBuffer(heapBuffer, offset: 0, index: 0)
  encoder.setBuffer(stackBuffer, offset: 0, index: 1)
  encoder.setBuffer(bookBuffer, offset: 0, index: 2)
  encoder.setBuffer(itrsBuffer, offset: 0, index: 3)
  encoder.setBuffer(outputsBuffer, offset: 0, index: 4)
  encoder.setBuffer(bookSizeBuffer, offset: 0, index: 5)
  encoder.setBuffer(mainRefBuffer, offset: 0, index: 6)
  encoder.setBuffer(numThreadsBuffer, offset: 0, index: 7)
  encoder.setBuffer(heapPerThrBuffer, offset: 0, index: 8)
  encoder.setBuffer(stackPerThrBuffer, offset: 0, index: 9)
  encoder.setBuffer(simdWidthBuffer, offset: 0, index: 10)

  let threadsPerGroup = min(numThreads, pipeline.maxTotalThreadsPerThreadgroup)
  encoder.dispatchThreads(MTLSize(width: numThreads, height: 1, depth: 1),
                          threadsPerThreadgroup: MTLSize(width: threadsPerGroup, height: 1, depth: 1))
  encoder.endEncoding()

  let start = DispatchTime.now()
  cmdBuffer.commit()
  cmdBuffer.waitUntilCompleted()
  let end = DispatchTime.now()

  let wallTime = Double(end.uptimeNanoseconds - start.uptimeNanoseconds) / 1_000_000_000.0

  // Sum iterations from all threads
  let itrsPtr = itrsBuffer.contents().bindMemory(to: UInt64.self, capacity: numThreads)
  var totalItrs: UInt64 = 0
  for i in 0..<numThreads {
    totalItrs += itrsPtr[i]
  }

  let mips = Double(totalItrs) / wallTime / 1_000_000.0

  // Get thread 0's result for display
  let outputsPtr = outputsBuffer.contents().bindMemory(to: Term.self, capacity: numThreads)
  let resultTerm = outputsPtr[0]

  print("Threads: \(numThreads)")
  print("Result (thread 0): tag=\(tag(resultTerm)) val=\(val(resultTerm))")
  print("Total interactions: \(totalItrs)")
  print("Time: \(String(format: "%.6f", wallTime))s")
  print("Performance: \(String(format: "%.2f", mips)) MIPS")
  print("Heap per thread: \(heapPerThr) terms (\(heapPerThr * 8 / 1024 / 1024) MB)")
  print("Stack per thread: \(stackPerThr) entries (\(stackPerThr * 8 / 1024 / 1024) MB)")
}

func runBenchmarkSuite(_ device: MTLDevice) {
  // Allocate host memory for parsing
  HEAP = UnsafeMutablePointer<Term>.allocate(capacity: HEAP_CAP)
  HEAP.initialize(repeating: 0, count: HEAP_CAP)
  BOOK = UnsafeMutablePointer<UInt32>.allocate(capacity: BOOK_CAP)
  BOOK.initialize(repeating: 0, count: BOOK_CAP)

  // Default benchmark
  let testSrc = """
    @ctru = λt.λf.t
    @cfal = λt.λf.f
    @cnot = λx.x(@cfal,@ctru)
    @P24  = λf.
      ! F &A = f;
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      ! F &A = λk. F₀(F₁(k));
      λk. F₀(F₁(k))
    @main = @P24(@cnot,@ctru)
    """

  var s = PState(file: "<inline>", src: testSrc, pos: 0, line: 1, col: 1)
  parseDef(&s)

  let bookSize = UInt32(ALLOC)

  var mainName: UInt32 = 0
  for c in "main" {
    mainName = ((mainName << 6) + UInt32(charToB64(c))) & UInt32(EXT_MASK)
  }

  guard BOOK[Int(mainName)] != 0 else {
    fputs("Error: @main not found\n", stderr)
    exit(1)
  }

  guard let shaderSource = loadShaderSource() else {
    fputs("Error: Could not load hvm4.metal shader\n", stderr)
    exit(1)
  }

  print("=== HVM4 Metal Benchmark Suite ===")
  print("Device: \(device.name)")
  print("")
  print("Threads\tTime(s)\t\tIterations\tMIPS")
  print("-------\t-------\t\t----------\t----")

  // Test with 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024 threads
  let threadCounts = [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024]

  for numThreads in threadCounts {
    let result = runBenchmarkOnce(device, shaderSource, bookSize, mainName, numThreads)
    print("\(numThreads)\t\(String(format: "%.6f", result.time))\t\(result.itrs)\t\t\(String(format: "%.2f", result.mips))")
  }
}

struct BenchmarkResult {
  let time: Double
  let itrs: UInt64
  let mips: Double
}

func runBenchmarkOnce(_ device: MTLDevice, _ shaderSource: String, _ bookSize: UInt32, _ mainName: UInt32, _ numThreads: Int) -> BenchmarkResult {
  let library: MTLLibrary
  do {
    library = try device.makeLibrary(source: shaderSource, options: nil)
  } catch {
    fputs("Error compiling shader: \(error)\n", stderr)
    exit(1)
  }

  let kernelName = numThreads == 1 ? "hvm_run" : "hvm_run_mt"
  guard let function = library.makeFunction(name: kernelName) else {
    fputs("Error: Could not find \(kernelName) kernel\n", stderr)
    exit(1)
  }

  let pipeline: MTLComputePipelineState
  do {
    pipeline = try device.makeComputePipelineState(function: function)
  } catch {
    fputs("Error creating pipeline: \(error)\n", stderr)
    exit(1)
  }

  guard let queue = device.makeCommandQueue() else {
    fputs("Error: Could not create command queue\n", stderr)
    exit(1)
  }

  if numThreads == 1 {
    // Single-threaded
    let heapBuffer = device.makeBuffer(bytes: HEAP, length: HEAP_CAP * MemoryLayout<Term>.stride, options: .storageModeShared)!
    let stackBuffer = device.makeBuffer(length: STACK_CAP * MemoryLayout<Term>.stride, options: .storageModeShared)!
    let bookBuffer = device.makeBuffer(bytes: BOOK, length: BOOK_CAP * MemoryLayout<UInt32>.stride, options: .storageModeShared)!
    let resultBuffer = device.makeBuffer(length: 5 * MemoryLayout<UInt32>.stride, options: .storageModeShared)!

    var initAlloc = bookSize
    let initAllocBuffer = device.makeBuffer(bytes: &initAlloc, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!
    var mainRef = mainName
    let mainRefBuffer = device.makeBuffer(bytes: &mainRef, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!

    // Warm up
    for _ in 0..<3 {
      memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)
      guard let cmdBuffer = queue.makeCommandBuffer(), let encoder = cmdBuffer.makeComputeCommandEncoder() else { exit(1) }
      encoder.setComputePipelineState(pipeline)
      encoder.setBuffer(heapBuffer, offset: 0, index: 0)
      encoder.setBuffer(stackBuffer, offset: 0, index: 1)
      encoder.setBuffer(bookBuffer, offset: 0, index: 2)
      encoder.setBuffer(resultBuffer, offset: 0, index: 3)
      encoder.setBuffer(initAllocBuffer, offset: 0, index: 4)
      encoder.setBuffer(mainRefBuffer, offset: 0, index: 5)
      encoder.dispatchThreads(MTLSize(width: 1, height: 1, depth: 1), threadsPerThreadgroup: MTLSize(width: 1, height: 1, depth: 1))
      encoder.endEncoding()
      cmdBuffer.commit()
      cmdBuffer.waitUntilCompleted()
    }

    // Benchmark
    memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)
    guard let cmdBuffer = queue.makeCommandBuffer(), let encoder = cmdBuffer.makeComputeCommandEncoder() else { exit(1) }
    encoder.setComputePipelineState(pipeline)
    encoder.setBuffer(heapBuffer, offset: 0, index: 0)
    encoder.setBuffer(stackBuffer, offset: 0, index: 1)
    encoder.setBuffer(bookBuffer, offset: 0, index: 2)
    encoder.setBuffer(resultBuffer, offset: 0, index: 3)
    encoder.setBuffer(initAllocBuffer, offset: 0, index: 4)
    encoder.setBuffer(mainRefBuffer, offset: 0, index: 5)
    encoder.dispatchThreads(MTLSize(width: 1, height: 1, depth: 1), threadsPerThreadgroup: MTLSize(width: 1, height: 1, depth: 1))
    encoder.endEncoding()

    let start = DispatchTime.now()
    cmdBuffer.commit()
    cmdBuffer.waitUntilCompleted()
    let end = DispatchTime.now()

    let wallTime = Double(end.uptimeNanoseconds - start.uptimeNanoseconds) / 1_000_000_000.0
    let resultPtr = resultBuffer.contents().bindMemory(to: UInt32.self, capacity: 5)
    let totalItrs = (UInt64(resultPtr[2]) << 32) | UInt64(resultPtr[1])
    let mips = Double(totalItrs) / wallTime / 1_000_000.0

    return BenchmarkResult(time: wallTime, itrs: totalItrs, mips: mips)
  } else {
    // Multi-threaded with CUDA-style warp-coalesced memory layout
    // Memory layout: [book | warp0 slice | warp1 slice | ...]
    // Each warp slice = heap_per_thr * simd_width terms, with interleaved access
    let simdWidth = pipeline.threadExecutionWidth
    let heapPerThr = MT_HEAP_PER_THR
    let stackPerThr = MT_STACK_PER_THR
    let numWarps = (numThreads + simdWidth - 1) / simdWidth
    let warpSliceSize = Int(heapPerThr) * simdWidth
    let totalHeapSize = Int(bookSize) + numWarps * warpSliceSize
    let totalStackSize = numThreads * Int(stackPerThr)

    let heapBuffer = device.makeBuffer(length: totalHeapSize * MemoryLayout<Term>.stride, options: .storageModeShared)!
    memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)
    let stackBuffer = device.makeBuffer(length: totalStackSize * MemoryLayout<Term>.stride, options: .storageModeShared)!
    let bookBuffer = device.makeBuffer(bytes: BOOK, length: BOOK_CAP * MemoryLayout<UInt32>.stride, options: .storageModeShared)!
    let itrsBuffer = device.makeBuffer(length: numThreads * MemoryLayout<UInt64>.stride, options: .storageModeShared)!
    let outputsBuffer = device.makeBuffer(length: numThreads * MemoryLayout<Term>.stride, options: .storageModeShared)!

    var bookSizeVal = bookSize
    let bookSizeBuffer = device.makeBuffer(bytes: &bookSizeVal, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!
    var mainRef = mainName
    let mainRefBuffer = device.makeBuffer(bytes: &mainRef, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!
    var numThreadsVal = UInt32(numThreads)
    let numThreadsBuffer = device.makeBuffer(bytes: &numThreadsVal, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!
    var heapPerThrVal = heapPerThr
    let heapPerThrBuffer = device.makeBuffer(bytes: &heapPerThrVal, length: MemoryLayout<UInt64>.stride, options: .storageModeShared)!
    var stackPerThrVal = stackPerThr
    let stackPerThrBuffer = device.makeBuffer(bytes: &stackPerThrVal, length: MemoryLayout<UInt64>.stride, options: .storageModeShared)!
    var simdWidthVal = UInt32(simdWidth)
    let simdWidthBuffer = device.makeBuffer(bytes: &simdWidthVal, length: MemoryLayout<UInt32>.stride, options: .storageModeShared)!

    let threadsPerGroup = min(numThreads, pipeline.maxTotalThreadsPerThreadgroup)

    // Warm up
    for _ in 0..<3 {
      memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)
      guard let cmdBuffer = queue.makeCommandBuffer(), let encoder = cmdBuffer.makeComputeCommandEncoder() else { exit(1) }
      encoder.setComputePipelineState(pipeline)
      encoder.setBuffer(heapBuffer, offset: 0, index: 0)
      encoder.setBuffer(stackBuffer, offset: 0, index: 1)
      encoder.setBuffer(bookBuffer, offset: 0, index: 2)
      encoder.setBuffer(itrsBuffer, offset: 0, index: 3)
      encoder.setBuffer(outputsBuffer, offset: 0, index: 4)
      encoder.setBuffer(bookSizeBuffer, offset: 0, index: 5)
      encoder.setBuffer(mainRefBuffer, offset: 0, index: 6)
      encoder.setBuffer(numThreadsBuffer, offset: 0, index: 7)
      encoder.setBuffer(heapPerThrBuffer, offset: 0, index: 8)
      encoder.setBuffer(stackPerThrBuffer, offset: 0, index: 9)
      encoder.setBuffer(simdWidthBuffer, offset: 0, index: 10)
      encoder.dispatchThreads(MTLSize(width: numThreads, height: 1, depth: 1), threadsPerThreadgroup: MTLSize(width: threadsPerGroup, height: 1, depth: 1))
      encoder.endEncoding()
      cmdBuffer.commit()
      cmdBuffer.waitUntilCompleted()
    }

    // Benchmark
    memcpy(heapBuffer.contents(), HEAP, Int(bookSize) * MemoryLayout<Term>.stride)
    guard let cmdBuffer = queue.makeCommandBuffer(), let encoder = cmdBuffer.makeComputeCommandEncoder() else { exit(1) }
    encoder.setComputePipelineState(pipeline)
    encoder.setBuffer(heapBuffer, offset: 0, index: 0)
    encoder.setBuffer(stackBuffer, offset: 0, index: 1)
    encoder.setBuffer(bookBuffer, offset: 0, index: 2)
    encoder.setBuffer(itrsBuffer, offset: 0, index: 3)
    encoder.setBuffer(outputsBuffer, offset: 0, index: 4)
    encoder.setBuffer(bookSizeBuffer, offset: 0, index: 5)
    encoder.setBuffer(mainRefBuffer, offset: 0, index: 6)
    encoder.setBuffer(numThreadsBuffer, offset: 0, index: 7)
    encoder.setBuffer(heapPerThrBuffer, offset: 0, index: 8)
    encoder.setBuffer(stackPerThrBuffer, offset: 0, index: 9)
    encoder.setBuffer(simdWidthBuffer, offset: 0, index: 10)
    encoder.dispatchThreads(MTLSize(width: numThreads, height: 1, depth: 1), threadsPerThreadgroup: MTLSize(width: threadsPerGroup, height: 1, depth: 1))
    encoder.endEncoding()

    let start = DispatchTime.now()
    cmdBuffer.commit()
    cmdBuffer.waitUntilCompleted()
    let end = DispatchTime.now()

    let wallTime = Double(end.uptimeNanoseconds - start.uptimeNanoseconds) / 1_000_000_000.0
    let itrsPtr = itrsBuffer.contents().bindMemory(to: UInt64.self, capacity: numThreads)
    var totalItrs: UInt64 = 0
    for i in 0..<numThreads {
      totalItrs += itrsPtr[i]
    }
    let mips = Double(totalItrs) / wallTime / 1_000_000.0

    return BenchmarkResult(time: wallTime, itrs: totalItrs, mips: mips)
  }
}

main()
