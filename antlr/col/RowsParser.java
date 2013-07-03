// Generated from Rows.g4 by ANTLR 4.0
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class RowsParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		TAB=1, NL=2, STUFF=3;
	public static final String[] tokenNames = {
		"<INVALID>", "'\t'", "NL", "STUFF"
	};
	public static final int
		RULE_file = 0, RULE_row = 1;
	public static final String[] ruleNames = {
		"file", "row"
	};

	@Override
	public String getGrammarFileName() { return "Rows.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public ATN getATN() { return _ATN; }


	  int col;
	  public RowsParser(TokenStream input, int col) {
	    this(input);
	    this.col = col;
	  }


	public RowsParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class FileContext extends ParserRuleContext {
		public RowContext row(int i) {
			return getRuleContext(RowContext.class,i);
		}
		public List<TerminalNode> NL() { return getTokens(RowsParser.NL); }
		public List<RowContext> row() {
			return getRuleContexts(RowContext.class);
		}
		public TerminalNode NL(int i) {
			return getToken(RowsParser.NL, i);
		}
		public FileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_file; }
	}

	public final FileContext file() throws RecognitionException {
		FileContext _localctx = new FileContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_file);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(7); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(4); row();
				setState(5); match(NL);
				}
				}
				setState(9); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==STUFF );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RowContext extends ParserRuleContext {
		public int i=0;
		public Token STUFF;
		public List<TerminalNode> STUFF() { return getTokens(RowsParser.STUFF); }
		public TerminalNode STUFF(int i) {
			return getToken(RowsParser.STUFF, i);
		}
		public RowContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_row; }
	}

	public final RowContext row() throws RecognitionException {
		RowContext _localctx = new RowContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_row);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(13); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(11); ((RowContext)_localctx).STUFF = match(STUFF);

				          _localctx.i++;
				          if (_localctx.i == col) System.out.println((((RowContext)_localctx).STUFF!=null?((RowContext)_localctx).STUFF.getText():null));
				          
				}
				}
				setState(15); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==STUFF );
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\2\3\5\24\4\2\t\2\4\3\t\3\3\2\3\2\3\2\6\2\n\n\2\r\2\16\2\13\3\3\3\3\6"+
		"\3\20\n\3\r\3\16\3\21\3\3\2\4\2\4\2\2\23\2\t\3\2\2\2\4\17\3\2\2\2\6\7"+
		"\5\4\3\2\7\b\7\4\2\2\b\n\3\2\2\2\t\6\3\2\2\2\n\13\3\2\2\2\13\t\3\2\2\2"+
		"\13\f\3\2\2\2\f\3\3\2\2\2\r\16\7\5\2\2\16\20\b\3\1\2\17\r\3\2\2\2\20\21"+
		"\3\2\2\2\21\17\3\2\2\2\21\22\3\2\2\2\22\5\3\2\2\2\4\13\21";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}