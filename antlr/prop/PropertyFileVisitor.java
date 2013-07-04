// Generated from PropertyFile.g4 by ANTLR 4.0
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.Token;

public interface PropertyFileVisitor<T> extends ParseTreeVisitor<T> {
	T visitFile(PropertyFileParser.FileContext ctx);

	T visitProp(PropertyFileParser.PropContext ctx);
}