#!/usr/bin/env swift

class HTMLElement {
    let name: String
    let text: String?

    lazy var asHTML: Void -> String = {
        if let text = self.text {
            return "<\(self.name)>\(text)</\(self.name)"
        } else {
            return "<\(self.name) />"
        }
    }

    init (name: String, text: String? = nil) {
        self.name = name
        self.text = text
    }

    deinit {
        print("\(name) is being deinitialized")
    }
}

let heading = HTMLElement(name: "hi")
let defText = "some default text"
heading.asHTML = {
    return "<\(heading.name)>\(heading.text ?? defText)</\(heading.name)>"
}

print(heading.asHTML())
