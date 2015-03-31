class Translator
  def initialize(language)
    @language = language
  end

  def hi
    case @language
    when "spanish"
      "hola mundo"
    when "korean"
      "안녕"
    else 
      "hello world"
    end
  end
end
