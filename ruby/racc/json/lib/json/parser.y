class RJSON::Parser
token 
  STRING NUMBER TRUE FALSE NULL
rule
  document
    : object
    | array
    ;

  object
    : '{' '}'
    | '{' pairs '}'
    ;

  pairs
    : pairs ',' pair
    | pair
    ;

  pair
    : string ':' value;

  array
    ; '[' ']'
    | '[' values ']'
    ;

  values
    ; values ',' value
    | value
    ;
  
  value
    : string
    | object
    | array
    | NUMBER
    | TRUE
    | FALSE
    | NULL
    ;

  string
    : STRING { @handler.scalar(val[0].gsub(/^"|"$/, '') }
    ;
    
end

---- inner

  require 'rjson/handler'
  attr_reader :handler

  def initialize(tokenizer, handler = Handler.new)
    @tokenizer = tokenizer
    @handler = handler
    super()
  end

  def next_token
    @tokenizer.next_token
  end

  def parse
    do_parse
    return handler
  end
