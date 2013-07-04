// Generated from PropertyFile.g4 by ANTLR 4.0
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class PropertyFileLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__1=1, T__0=2, ID=3, STRING=4;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'\n'", "'='", "ID", "STRING"
	};
	public static final String[] ruleNames = {
		"T__1", "T__0", "ID", "STRING"
	};


	public PropertyFileLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "PropertyFile.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\2\4\6\35\b\1\4\2\t\2\4\3\t\3\4\4\t\4\4\5\t\5\3\2\3\2\3\3\3\3\3\4\6\4"+
		"\21\n\4\r\4\16\4\22\3\5\3\5\7\5\27\n\5\f\5\16\5\32\13\5\3\5\3\5\3\30\6"+
		"\3\3\1\5\4\1\7\5\1\t\6\1\3\2\3\3c|\36\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2"+
		"\2\2\2\t\3\2\2\2\3\13\3\2\2\2\5\r\3\2\2\2\7\20\3\2\2\2\t\24\3\2\2\2\13"+
		"\f\7\f\2\2\f\4\3\2\2\2\r\16\7?\2\2\16\6\3\2\2\2\17\21\t\2\2\2\20\17\3"+
		"\2\2\2\21\22\3\2\2\2\22\20\3\2\2\2\22\23\3\2\2\2\23\b\3\2\2\2\24\30\7"+
		"$\2\2\25\27\13\2\2\2\26\25\3\2\2\2\27\32\3\2\2\2\30\31\3\2\2\2\30\26\3"+
		"\2\2\2\31\33\3\2\2\2\32\30\3\2\2\2\33\34\7$\2\2\34\n\3\2\2\2\5\2\22\30";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
	}
}