import org.antlr.v4.misc.OrderedHashMap;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

import java.io.*;
import java.util.Map;

public class TestPropertyFileVisitor {
    public static class PropertyFileVisitor extends PropertyFileBaseVisitor<Void> {
        Map<String, String> props = new OrderedHashMap<String, String>();
        public Void visitProp(PropertyFileVisitor.PropContext ctx) {
            String id = ctx.ID().getText();
            String value = ctx.STRING().getText();
            props.put(id, value);
            return null;
        }
    }

    public static void main(String[] args) throws Exception {
        String inputFile = null;
        if ( args.length>0 ) inputFile = args[0];
        InputStream is = System.in;
        if ( inputFile!=null ) { 
            is = new FileInputStream(inputFile);
        }   
        ANTLRInputStream input = new ANTLRInputStream(is);
        PropertyFileLexer lexer = new PropertyFileLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        PropertyFileParser parser = new PropertyFileParser(tokens);
        ParseTree tree = parser.file();

        PropertyFileVisitor loader = new PropertyFileVisitor();
        loader.visit(tree);
        System.out.println(loader.props); // print results
    }
}

