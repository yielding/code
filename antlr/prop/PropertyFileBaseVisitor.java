// Generated from PropertyFile.g4 by ANTLR 4.0
import org.antlr.v4.runtime.tree.*;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.ParserRuleContext;

public class PropertyFileBaseVisitor<T> extends AbstractParseTreeVisitor<T> implements PropertyFileVisitor<T> {
	@Override 
    public T visitFile(PropertyFileParser.FileContext ctx) { 
        return visitChildren(ctx); 
    }

	@Override 
    public T visitProp(PropertyFileParser.PropContext ctx) { 
        return visitChildren(ctx); 
    }
}
