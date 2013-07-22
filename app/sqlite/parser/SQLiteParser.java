import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;
import java.util.regex.*;

class Column {
    String name;
    String type; 
    int typeLen;

    boolean notNull, isPrimaryKey;

    public Column() {
        this.typeLen = 0;
        this.notNull = false;
        this.isPrimaryKey = false;
    }

    public void setName(String name)     { this.name = name;      }
    public void setType(String type)     { this.type = type;      }
    public void setTypeLength(int len)   { this.typeLen = len;    }

    public void setNotNull(boolean v)    { this.notNull = v;      }
    public void setPrimaryKey(boolean v) { this.isPrimaryKey = v; }

    public String toString() {
        return String.format("cn: %20s, type: %7s, len: %3d, NN: %2S, PRI: %2S",
                name, type, typeLen, 
                TF(notNull),
                TF(isPrimaryKey));
    }

    private String TF(boolean v) { return v ? "T" : "F"; }
}

class Table {
    private String name;
    private boolean isTemp;
    private List<Column> cols = new ArrayList<Column>();

    public Table() { 
        this.isTemp = false;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return this.name;
    }

    public void setTemporary(boolean v) {
        this.isTemp = v;
    }

    public void add(Column col) {
        cols.add(col);
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("table name: ").append(this.name);
        if (isTemp) buffer.append(" ,temporary");
        buffer.append("\n");

        for (Column col : cols) {
            buffer.append(col.toString());
            buffer.append("\n");
        }

        return buffer.toString();
    }
}

class SQLiteCreateTableListener extends CreateTableBaseListener {
    List<String> tableNames = new ArrayList<String>();

    List<Table> tables = new ArrayList<Table>();
    Table table;
    Column col;

    private boolean colDefMode = false;

    // table
    @Override 
    public void enterCreateTableStmt(CreateTableParser.CreateTableStmtContext ctx) { 
        table = new Table();
    }

    @Override public void exitTmp(CreateTableParser.TmpContext ctx) { 
        table.setTemporary(true);
    }

    @Override 
    public void exitTableName(CreateTableParser.TableNameContext ctx) { 
        table.setName(ctx.getText());
    }

    @Override 
    public void exitCreateTableStmt(CreateTableParser.CreateTableStmtContext ctx) { 
        tables.add(table);
        table = null;
    }

    // column
    @Override 
    public void enterColumnDefs(CreateTableParser.ColumnDefsContext ctx) { 
      colDefMode = true;
    }

    @Override 
    public void exitColumnDefs(CreateTableParser.ColumnDefsContext ctx) { 
      colDefMode = false;
    }

    @Override 
    public void enterColumnDef(CreateTableParser.ColumnDefContext ctx) { 
        if (colDefMode)
          col = new Column();
    }

    @Override 
    public void exitColumnName(CreateTableParser.ColumnNameContext ctx) { 
        if (colDefMode)
        {
            String name = ctx.getText();
            if (name.startsWith("'") || name.startsWith("\""))
                name = name.substring(1, name.length()-1);

            col.setName(name);
        }
    }

    @Override 
    public void exitTypeName(CreateTableParser.TypeNameContext ctx) { 
        col.setType(ctx.getText());
    }

    @Override 
    public void exitColumnConstraint(CreateTableParser.ColumnConstraintContext ctx) { 
        String text = ctx.getText().toUpperCase();
        //System.out.println(ctx.getChildCount() +  " : " + ctx.getText());

        if (text.matches(".*NOTNULL.*"))
            col.setNotNull(true);

        if (text.matches(".*PRIMARY.*"))
            col.setPrimaryKey(true);
    }

    @Override 
    public void exitColumnDef(CreateTableParser.ColumnDefContext ctx) { 
        table.add(col);
    }

    //    public void exitLength_constraint(CreateTableParser.Length_constraintContext ctx) { 
    //        if (col != null)
    //            col.setTypeLength(Integer.parseInt(ctx.NUMBER().getText()));
    //    }
    //

    public void printDBInfo() {
        for (Table t : tables)
            System.out.println(t);
    }
}

public class SQLiteParser {
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
        ParseTree tree = parser.tableList();

        ParseTreeWalker walker = new ParseTreeWalker();
        SQLiteCreateTableListener listener = new SQLiteCreateTableListener();
        walker.walk(listener, tree);
        listener.printDBInfo();
    }
};
