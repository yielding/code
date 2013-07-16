import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;


import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;

public class SQLiteParser {

    public static class SQLiteCreateTableListener extends CreateTableBaseListener {
        List<String> tables = new ArrayList<String>();

        public void exitTable_name(CreateTableParser.Table_nameContext ctx) { 
            String name = ctx.getText();
            tables.add(name);
        }

        public void tableNames() {
            for (String name : tables)
                System.out.println("table name: " + name);
        }
    }

    public static void main(String[] args) throws Exception {
        String inputFile = null;
        if (args.length > 0)
            inputFile = args[0];

        InputStream is = System.in;
        if (inputFile != null)
            is = new FileInputStream(inputFile);

        CreateTableLexer   lexer = new CreateTableLexer(new ANTLRInputStream(is));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CreateTableParser parser = new CreateTableParser(tokens);
        parser.setBuildParseTree(true);
        ParseTree tree = parser.table_list();


        ParseTreeWalker walker = new ParseTreeWalker();
        SQLiteCreateTableListener listener = new SQLiteCreateTableListener();
        walker.walk(listener, tree);
        listener.tableNames();
    }
};
