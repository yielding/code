" Vim syntax file
" Language:	ObjectPascal
" Previous Maintainer: (pascal.vim) Xavier Crégut <xavier.cregut@enseeiht.fr>	
" Current  Maintainer: Thorsten Maerz <info@netztorte.de>
" Last Change:	2001 May 26

" Remove any old syntax stuff hanging around
syn clear

syn case ignore

syn sync lines=250

if !exists("pascal_traditional")
endif

if !exists("did_pascal_syntax_inits")
  let did_pascal_syntax_inits = 1
  " The default methods for highlighting.  Can be overridden later
endif

" ------------------------------------------------------------------------------

syn keyword pascalTodo 			contained	TODO

" String
syn region  pascalString		start=+'+	end=+'+

"syn match  pascalIdentifier		"\<[a-zA-Z_][a-zA-Z0-9_]*\>"

syn match  pascalDelimiter		"[()]"

syn match  pascalMatrixDelimiter	"[][]"

"if you prefer you can highlight the range
"syn match  pascalMatrixDelimiter	"[\d\+\.\.\d\+]"

"syn match  pascalNumber		"-\=\<\d\+\.\d\+[dD]-\=\d\+\>"
"syn match  pascalNumber		"-\=\<\d\+\.\d\+[eE]-\=\d\+\>"
"syn match  pascalNumber		"-\=\<\d\+\.\d\+\>"
syn match  pascalNumber			"-\=\<\d\+\>"
syn match  pascalByte			"\$[0-9a-fA-F]\+\>"

" If you don't like tabs
"syn match pascalShowTab 		"\t"
"syn match pascalShowTabc 		"\t"

syn region pascalComment		start="(\*"	end="\*)"	contains=pascalTodo
syn region pascalComment		start="{"	end="}"		contains=pascalTodo
syn region pascalComment		start="//"	end="$"		contains=pascalTodo

syn keyword pascalLabel			case

syn keyword pascalAcces			public
syn keyword pascalAcces			private
syn keyword pascalAcces			protected
syn keyword pascalAcces			protected

syn match pascalAssignment		":="
syn keyword pascalAssignment		assign

syn keyword pascalConstant		true
syn keyword pascalConstant		false
syn keyword pascalConstant		nil
syn keyword pascalConstant		maxint

syn keyword pascalConditional		if
syn keyword pascalConditional		then
syn keyword pascalConditional		else
syn keyword pascalConditional		try
syn keyword pascalConditional		except
syn keyword pascalConditional		on
syn keyword pascalConditional		finally

syn keyword pascalRepeat		do
syn keyword pascalRepeat		for
syn keyword pascalRepeat		while
syn keyword pascalRepeat		to
syn keyword pascalRepeat		downto
syn keyword pascalRepeat		repeat
syn keyword pascalRepeat		until

syn keyword pascalComparator		is
syn keyword pascalComparator		in
syn match pascalComparator 		"="
syn match pascalComparator 		"<"
syn match pascalComparator 		">"

syn keyword pascalFunction		procedure
syn keyword pascalFunction		function
syn keyword pascalFunction		record
syn keyword pascalFunction		type
syn keyword pascalFunction		var
syn keyword pascalFunction		const
syn keyword pascalFunction		constructor destructor
syn keyword pascalFunction		property

syn keyword pascalOperator		and
syn keyword pascalOperator		not
syn keyword pascalOperator		or
syn keyword pascalOperator		div
syn keyword pascalOperator		mod
syn keyword pascalOperator		xor
syn keyword pascalOperator		shl
syn keyword pascalOperator		shr
syn keyword pascalOperator		as
syn keyword pascalOperator		of
syn keyword pascalOperator		with

syn keyword pascalStatement		raise
syn keyword pascalStatement		program
syn keyword pascalStatement		begin
syn keyword pascalStatement		end
syn keyword pascalStatement		goto
syn keyword pascalStatement		delete
syn keyword pascalStatement		new
syn keyword pascalStatement		dispose
syn keyword pascalStatement		get
syn keyword pascalStatement		put
syn keyword pascalStatement		insert
syn keyword pascalStatement		label
syn keyword pascalStatement		reset
syn keyword pascalStatement		rewrite
syn keyword pascalStatement		seek 
syn keyword pascalStatement		read
syn keyword pascalStatement		readln
syn keyword pascalStatement		write
syn keyword pascalStatement		writeln
syn keyword pascalStatement		asm
syn keyword pascalStatement		unit
syn keyword pascalStatement		interface
syn keyword pascalStatement		implementation
syn keyword pascalStatement		uses
syn keyword pascalStatement		inherited
syn keyword pascalStatement		forward
syn keyword pascalStatement		library exports
syn keyword pascalStatement		initialization finalization
syn keyword pascalStatement		package
syn keyword pascalStatement		contains
syn keyword pascalStatement		requires

syn keyword pascalType			object
syn keyword pascalType			class
syn keyword pascalType			set
syn keyword pascalType			boolean
syn keyword pascalType			char
syn keyword pascalType			integer
syn keyword pascalType			real
syn keyword pascalType			string
syn keyword pascalType			array
syn keyword pascalType			file
syn keyword pascalType			keyboard
syn keyword pascalType			input
syn keyword pascalType			output
syn keyword pascalType			resourcestring
syn keyword pascalType			threadvar
syn keyword pascalType			byte
syn keyword pascalType			word
syn keyword pascalType			dword
syn keyword pascalType			longint
syn keyword pascalType			cardinal
syn keyword pascalType			short
syn keyword pascalType			long
syn keyword pascalType			shortstring
syn keyword pascalType			longstring
syn keyword pascalType			float
syn keyword pascalType			extended
syn keyword pascalType			double
syn keyword pascalType			pointer
syn keyword pascalType			variant
syn keyword pascalType			pchar
syn keyword pascalType			TDateTime
syn keyword pascalType			TDate
syn keyword pascalType			TTime
syn keyword pascalType			THandle
syn keyword pascalType			TRect

syn keyword pascalObject		TObject TPersistent TComponent TControl TWinControl
syn keyword pascalObject		TForm TFrame
syn keyword pascalObject		TButton TBitBtn TSpeedBtn
syn keyword pascalObject		TLabel TStaticText
syn keyword pascalObject		TPanel TSplitter TGroupBox TRadioGroupBox
syn keyword pascalObject		TCheckBox TRadioButton
syn keyword pascalObject		TImage TImageList
syn keyword pascalObject		TTimer
syn keyword pascalObject		TEdit TMemo TListBox TComboBox TCheckListBox
syn keyword pascalObject		TMenu TMainMenu TPopupMenu TMenuItem
syn keyword pascalObject		TTabNoteBook TTabSheet TPageControl
syn keyword pascalObject		TUpDown TSpinEdit
syn keyword pascalObject		TAction TActionList
syn keyword pascalObject		TDrawGrid TStringGrid TDBGrid
syn keyword pascalObject		TList TStrings TStringList
syn keyword pascalObject		TException Exception
syn keyword pascalObject		TServerSocket TClientSocket TSocket
syn keyword pascalObject		TErrorEvent TNotifyEvent
syn keyword pascalObject		TStream TFileStream TMemoryStream TCustomStream TReader TWriter
syn keyword pascalObject		TField TTable TQuery TDataSource TDataSet TDataBase TParam TParams

"  virtual
syn keyword pascalModifier		inline
syn keyword pascalModifier		external
syn keyword pascalModifier		assembler
syn keyword pascalModifier		near
syn keyword pascalModifier		far
syn keyword pascalModifier		absolute
syn keyword pascalModifier		interrupt
syn keyword pascalModifier		interactive
syn keyword pascalModifier		packed
syn keyword pascalModifier		dispinterface
syn keyword pascalModifier		out
syn keyword pascalModifier		automated
syn keyword pascalModifier		at
syn keyword pascalModifier		default nodefault
syn keyword pascalModifier		dispid
syn keyword pascalModifier		implements
"syn keyword pascalModifier		index
syn keyword pascalModifier		message
syn keyword pascalModifier		readonly writeonly
syn keyword pascalModifier		resident
syn keyword pascalModifier		stored

syn keyword pascalObjModifier		abstract virtual override reintroduce

syn keyword pascalFuncModifier		cdecl pascal register stdcall safecall
syn keyword pascalFuncModifier		dynamic export
syn keyword pascalFuncModifier		name
syn keyword pascalFuncModifier		overload

" ------------------------------------------------------------------------------

hi link pascalAcces			Statement
hi link pascalByte			Number
hi link pascalComment			Comment
hi link pascalConditional		Conditional
hi link pascalFunction		Function
hi link pascalLabel			Label
hi link pascalMatrixDelimiter		Identifier
hi link pascalModifier		Type
hi link pascalNumber			Number
hi link pascalOperator		Operator
hi link pascalRepeat			Repeat
hi link pascalStatement		Statement
hi link pascalString			String
hi link pascalStructure		Structure
hi link pascalTodo			Todo
hi link pascalType			Type
hi link pascalUnclassified		Statement

hi link pascalObject			Type
hi link pascalFuncModifier		pascalModifier
hi link pascalObjModifier		pascalModifier

"optional highlighting
  "hi link pascalDelimiter		Identifier

  "hi link pascalShowTab		Error
  "hi link pascalShowTabc		Error

  "hi link pascalIdentifier		Identifier

hi pascalObject 		ctermfg=white 		guifg=white 		gui=italic
hi pascalFunction 		ctermfg=LightRed	guifg=Orange 		gui=NONE
hi pascalStatement 		ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalNumber 		ctermfg=LightBlue	guifg=LightBlue		gui=NONE
hi pascalByte 			ctermfg=LightRed 	guifg=LightRed 		gui=bold
hi pascalString 		ctermfg=LightBlue 	guifg=LightBlue 	gui=NONE
hi pascalMatrixDelimiter 	ctermfg=lightred 	guifg=lightred 		gui=NONE
hi pascalConditional 		ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalConstant 		ctermfg=white 		guifg=white 		gui=NONE
hi pascalModifier 		ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalType 			ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalStructure 		ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalRepeat 		ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalOperator 		ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalAssignment 		ctermfg=Magenta 	guifg=Magenta 		gui=NONE
hi pascalComment 		ctermfg=LightBlue 	guifg=#00A0A0 		gui=italic
hi pascalLabel	 		ctermfg=Green 		guifg=Green 		gui=NONE
hi pascalDelimiter 		ctermfg=Yellow 		guifg=Yellow 		gui=NONE
hi pascalComparator		ctermfg=Yellow		guifg=Yellow		gui=NONE

" ------------------------------------------------------------------------------

let b:current_syntax = "delphi"

" vim: ts=8 sw=2
