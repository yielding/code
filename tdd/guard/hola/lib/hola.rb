class Hola
  def self.hi(language = "korean")
    translator = Translator.new(language)
    translator.hi
  end
end

require 'hola/translator'
