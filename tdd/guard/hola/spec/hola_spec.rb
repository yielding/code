require 'hola'

describe Hola do
  context 'hola' do
    before :each do
    end

    it 'shoild translate to eng' do
      expect(Hola.hi("english")).to eq("hello world")
    end

    it 'shoild translate to korean' do
      expect(Hola.hi).to eq("안녕")
    end

    it 'shoild translate to korean' do
      expect(Hola.hi("korean")).to eq("안녕")
    end
  end
end
  
