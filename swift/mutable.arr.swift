import Foundation

////////////////////////////////////////////////////////////////////////////////
//
// []는 container.. map을 지원한다. 
// http://swiftdoc.org/v3.0/protocol/Collection/#subscript-subsequence-slice-self-subscript_-range-self-index
// NSData, NSMutableData는 지원하지 않는다.
//
////////////////////////////////////////////////////////////////////////////////
public class ByteBuffer {
  public var data: NSMutableData?

  var shallow: Bool = false
  var offset: Int = 0

  public init(data: [UInt8]) {
    self.data = NSMutableData(bytes: data, length: data.count)
  }

  public init(shallowData sd: [UInt8]) {
    data = NSMutableData(bytesNoCopy: UnsafeMutablePointer<UInt>(sd), length: sd.count)
    shallow = true
  }

  public init(contentsOfFile path: String) {
    data = NSMutableData(contentsOfFile: path)
  }

  public func getInt32LE() -> Int32 {
    var result: Int32 = 0
    data!.getBytes(&result, range: NSRange(location: offset, length: 4))
    offset += 4

    return result
  }

  public func getInt32BE() -> Int32 {
    return Int32(bigEndian: self.getInt32LE())
  }

  public func getInt16LE() -> Int16 {
    var result: Int16 = 0
    data!.getBytes(&result, range: NSRange(location: offset, length: 2))
    offset += 2

    return result
  }

  public func getInt16BE() -> Int16 {
    return Int16(bigEndian: self.getInt16LE())
  }

  public func hasRemaining() -> Bool {
    return offset < data!.length
  }

  public func getBytes(count c: Int) -> [UInt8]? {
    if offset + c >= data!.length { return nil }

    var result = [UInt8](repeating: 0, count: c)
    data!.getBytes(&result, range: NSRange(location: offset, length: c))
    offset += c

    return result
  }

  public func flip() {
    offset = 0
  }

  public func toHex() -> String {
    let bb = [UInt8](UnsafeBufferPointer(start: UnsafePointer(data!.bytes), count: data!.length))

    return bb.map {
      ($0 < 16 ? "0" : "") + String($0, radix: 16, uppercase: false)
    }.joined(separator: "")
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
public class ByteBuffer2 {
  public var data: [UInt8]

  var shallow: Bool = false
  var offset: Int = 0

  public init(data: [UInt8]) {
    self.data = data
  }

  public init(contentsOfFile path: String) {
    let d = NSMutableData(contentsOfFile: path)
    self.data = [UInt8](UnsafeBufferPointer(start: UnsafePointer(d!.bytes), count: d!.length))
  }

  public func getInt32LE() -> Int32 {
    let result = data[offset..<offset+4].reversed().reduce(0) { $0 * 16 + Int32($1) }
    offset += 4

    return result
  }

  public func getInt32BE() -> Int32 {
    // guard offset + 4 < data.count else { throw }
    let result = data[offset..<offset+4].reduce(0) { $0 * 16 + Int32($1) }
    offset += 4

    return result
  }

  public func getInt16LE() -> Int16 {
    let result = data[offset..<offset+2].reduce(0) { $0 * 16 + Int16($1) }
    offset += 2

    return result
  }

  public func getInt16BE() -> Int16 {
    let result = data[offset..<offset+2].reversed().reduce(0) { $0 * 16 + Int16($1) }
    offset += 2

    return result
  }

  public func hasRemaining() -> Bool {
    return offset < data.count
  }

  public func getBytes(count c: Int) -> [UInt8] {
    // guard offset + c < data.count else { return nil }
    let result: [UInt8] = [UInt8](data[offset..<offset+c])
    offset += c

    return result
  }

  public func flip()  { offset = 0 }

  public func reset() { offset = 0 }

  public func toHex() -> String {
    return data.map {
      ($0 < 16 ? "0" : "") + String($0, radix: 16, uppercase: false)
    }.joined(separator: "")
  }

  // private func getIntLE<T: SignedInteger where T: SignedInteger>() -> T {
  //   return data[offset..<offset + sizeof(T)].reversed().reduce(0) { $0 * 16 + T($1) }
  // }
}

////////////////////////////////////////////////////////////////////////////////
//
// Block
//
////////////////////////////////////////////////////////////////////////////////
public enum DataSource: Int {
  case DB = 1
  case RBJ, WAL
}

public class Block {
  public var Offset: Int = 0
  public var Size: Int = 0
  public var Source: DataSource = DataSource.DB

  public var Data: [UInt8]

  public var description: String {
    get { return "size: \(Size), offset: \(Offset)" }
  }

  public init(data: [UInt8], offset: Int, size: Int = -1, source: DataSource = DataSource.DB) {
    Data   = data
    Offset = offset
    Size   = size
    Source = source
  }

  public func IsInteriorIndex() -> Bool { return Data[0] == 0x02 }
  public func IsInteriorTable() -> Bool { return Data[0] == 0x05 }
  public func IsLeafIndex() -> Bool { return Data[0] == 0x0a }
  public func IsLeafTable() -> Bool { return Data[0] == 0x0d }
}

////////////////////////////////////////////////////////////////////////////////
//
// ByteBuffer
//
////////////////////////////////////////////////////////////////////////////////
// var d0: [UInt8] = [0x00, 0x01, 0x00, 0x02]
// var b1 = ByteBuffer(data: d0)
// 
// while b1.hasRemaining() { print(b1.getInt16LE()) }
// b1.flip()
// 
// while b1.hasRemaining() { print(b1.getInt16BE()) }
// 
// b1.flip()
// print(b1.toHex())
// 
// print(b1.getBytes(count: 300))
// 
// b1.flip()
// print(String(b1.getInt32BE(), radix: 16, uppercase: false))

////////////////////////////////////////////////////////////////////////////////
//
// ByteBuffer2
//
////////////////////////////////////////////////////////////////////////////////
var d0: [UInt8] = [0x00, 0x01, 0x00, 0x02]
var b1 = ByteBuffer2(data: d0)

b1.flip()
print(String(b1.getInt32LE(), radix: 16, uppercase: false))

b1.flip()
print(String(b1.getInt32BE(), radix: 16, uppercase: false))

b1.flip()
print(b1.getBytes(count: 2))
