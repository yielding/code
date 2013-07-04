// Generated from Data.g4 by ANTLR 4.0
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class DataParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		INT=1, WS=2;
	public static final String[] tokenNames = {
		"<INVALID>", "INT", "WS"
	};
	public static final int
		RULE_file = 0, RULE_group = 1, RULE_sequence = 2;
	public static final String[] ruleNames = {
		"file", "group", "sequence"
	};

	@Override
	public String getGrammarFileName() { return "Data.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public DataParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class FileContext extends ParserRuleContext {
		public GroupContext group(int i) {
			return getRuleContext(GroupContext.class,i);
		}
		public List<GroupContext> group() {
			return getRuleContexts(GroupContext.class);
		}
		public FileContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_file; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DataListener ) ((DataListener)listener).enterFile(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DataListener ) ((DataListener)listener).exitFile(this);
		}
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
				setState(6); group();
				}
				}
				setState(9); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( _la==INT );
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

	public static class GroupContext extends ParserRuleContext {
		public Token INT;
		public TerminalNode INT() { return getToken(DataParser.INT, 0); }
		public SequenceContext sequence() {
			return getRuleContext(SequenceContext.class,0);
		}
		public GroupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_group; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DataListener ) ((DataListener)listener).enterGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DataListener ) ((DataListener)listener).exitGroup(this);
		}
	}

	public final GroupContext group() throws RecognitionException {
		GroupContext _localctx = new GroupContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_group);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(11); ((GroupContext)_localctx).INT = match(INT);
			setState(12); sequence((((GroupContext)_localctx).INT!=null?Integer.valueOf(((GroupContext)_localctx).INT.getText()):0));
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

	public static class SequenceContext extends ParserRuleContext {
		public int n;
		public int i = 1;;
		public List<TerminalNode> INT() { return getTokens(DataParser.INT); }
		public TerminalNode INT(int i) {
			return getToken(DataParser.INT, i);
		}
		public SequenceContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public SequenceContext(ParserRuleContext parent, int invokingState, int n) {
			super(parent, invokingState);
			this.n = n;
		}
		@Override public int getRuleIndex() { return RULE_sequence; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof DataListener ) ((DataListener)listener).enterSequence(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof DataListener ) ((DataListener)listener).exitSequence(this);
		}
	}

	public final SequenceContext sequence(int n) throws RecognitionException {
		SequenceContext _localctx = new SequenceContext(_ctx, getState(), n);
		enterRule(_localctx, 4, RULE_sequence);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(19);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					{
					{
					setState(14);
					if (!(_localctx.i<=_localctx.n)) throw new FailedPredicateException(this, "$i<=$n");
					setState(15); match(INT);
					_localctx.i++;
					}
					} 
				}
				setState(21);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
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

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 2: return sequence_sempred((SequenceContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean sequence_sempred(SequenceContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0: return _localctx.i<=_localctx.n;
		}
		return true;
	}

	public static final String _serializedATN =
		"\2\3\4\31\4\2\t\2\4\3\t\3\4\4\t\4\3\2\6\2\n\n\2\r\2\16\2\13\3\3\3\3\3"+
		"\3\3\4\3\4\3\4\7\4\24\n\4\f\4\16\4\27\13\4\3\4\2\5\2\4\6\2\2\27\2\t\3"+
		"\2\2\2\4\r\3\2\2\2\6\25\3\2\2\2\b\n\5\4\3\2\t\b\3\2\2\2\n\13\3\2\2\2\13"+
		"\t\3\2\2\2\13\f\3\2\2\2\f\3\3\2\2\2\r\16\7\3\2\2\16\17\5\6\4\2\17\5\3"+
		"\2\2\2\20\21\6\4\2\3\21\22\7\3\2\2\22\24\b\4\1\2\23\20\3\2\2\2\24\27\3"+
		"\2\2\2\25\23\3\2\2\2\25\26\3\2\2\2\26\7\3\2\2\2\27\25\3\2\2\2\4\13\25";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}