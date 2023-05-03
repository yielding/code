public struct Blob {

	public let bytes: [UInt8]

	public init(bytes: [UInt8]) {
		self.bytes = bytes
	}

	public init(bytes: UnsafePointer<Void>, length: Int) {
		self.init(bytes: [UInt8](UnsafeBufferPointer(
			start: UnsafePointer(bytes), count: length
			)))
	}

	public func toHex() -> String {
		return bytes.map {
			($0 < 16 ? "0" : "") + String($0, radix: 16, uppercase: false)
		}.joined(separator: "")
	}
}

extension Blob : CustomStringConvertible {
	public var description: String {
		return "x'\(toHex())'"
	}
}

extension Blob : Equatable {
}

public func == (lhs: Blob, rhs: Blob) -> Bool {
	return lhs.bytes == rhs.bytes
}
