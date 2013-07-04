// Generated from PropertyFile.g4 by ANTLR 4.0
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.Token;

public interface PropertyFileListener extends ParseTreeListener {
	void enterFile(PropertyFileParser.FileContext ctx);
	void exitFile(PropertyFileParser.FileContext ctx);

	void enterProp(PropertyFileParser.PropContext ctx);
	void exitProp(PropertyFileParser.PropContext ctx);
}