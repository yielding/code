// Generated from Rows.g4 by ANTLR 4.0
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class RowsLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		TAB=1, NL=2, STUFF=3;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'\t'", "NL", "STUFF"
	};
	public static final String[] ruleNames = {
		"TAB", "NL", "STUFF"
	};


	public RowsLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Rows.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 0: TAB_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void TAB_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip();  break;
		}
	}

	public static final String _serializedATN =
		"\2\4\5\27\b\1\4\2\t\2\4\3\t\3\4\4\t\4\3\2\3\2\3\2\3\2\3\3\5\3\17\n\3\3"+
		"\3\3\3\3\4\6\4\24\n\4\r\4\16\4\25\2\5\3\3\2\5\4\1\7\5\1\3\2\3\4\13\f\17"+
		"\17\30\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\3\t\3\2\2\2\5\16\3\2\2\2\7"+
		"\23\3\2\2\2\t\n\7\13\2\2\n\13\3\2\2\2\13\f\b\2\2\2\f\4\3\2\2\2\r\17\7"+
		"\17\2\2\16\r\3\2\2\2\16\17\3\2\2\2\17\20\3\2\2\2\20\21\7\f\2\2\21\6\3"+
		"\2\2\2\22\24\n\2\2\2\23\22\3\2\2\2\24\25\3\2\2\2\25\23\3\2\2\2\25\26\3"+
		"\2\2\2\26\b\3\2\2\2\5\2\16\25";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}