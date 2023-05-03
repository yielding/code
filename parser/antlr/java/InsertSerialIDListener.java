import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.TokenStreamRewriter;

public class InsertSerialIDListener extends JavaBaseListener {
  TokenStreamRewriter rewriter;
  public InsertSerialIDListener(TokenStream tokens) {
    rewriter = new TokenStreamRewriter(tokens);
  }

  @Override
  public void enterClassBody(JavaParser.ClassBodyContext ctx) {
    String field = "\n\tpublic static final long serialVersionUID = 1L;";
    rewriter.insertAfter(ctx.start, field);
  }
}
