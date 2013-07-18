import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;

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

    public void setName(String name)   { this.name = name;   }
    public void setType(String type)   { this.type = type;   }
    public void setTypeLength(int len) { this.typeLen = len; }

    public void setNotNull(boolean v)  { this.notNull = v;   }
    public void setPrimaryKey(boolean v)  { this.isPrimaryKey = v;   }


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

//    public void enterTable_element(CreateTableParser.Table_elementContext ctx) { 
//        col = new Column();
//    }
//
//    public void exitColumn_name(CreateTableParser.Column_nameContext ctx) { 
//        if (col != null)
//            col.setName(ctx.getText());
//    }
//
//    public void exitColumn_constraint(CreateTableParser.Column_constraintContext ctx) {
//        if (col == null)
//            return;
//
//        if (ctx.NOT() != null)
//            col.setNotNull(true);
//
//        if (ctx.PRIMARY() != null)
//            col.setPrimaryKey(true);
//    }
//
//    public void exitData_type(CreateTableParser.Data_typeContext ctx) { 
//        if (col != null)
//            col.setType(ctx.getText());
//    }
//
//    public void exitLength_constraint(CreateTableParser.Length_constraintContext ctx) { 
//        if (col != null)
//            col.setTypeLength(Integer.parseInt(ctx.NUMBER().getText()));
//    }
//
//    public void exitTable_element(CreateTableParser.Table_elementContext ctx) { 
//        table.add(col);
//    }
//
//    public void exitTmp(CreateTableParser.TmpContext ctx) { 
//        if (ctx.TEMP() != null || ctx.TEMPORARY() != null)
//            table.setTemporary(true);
//    }
//
//    public void enterTable_def(CreateTableParser.Table_defContext ctx) { 
//        table = new Table();
//    }
//
//    public void exitTable_name(CreateTableParser.Table_nameContext ctx) { 
//        table.setName(ctx.getText());
//    }
//
//    public void exitTable_def(CreateTableParser.Table_defContext ctx) { 
//        tables.add(table);
//        table = null;
//    }

//public void enterTable_list(CreateTableParser.Table_listContext ctx) { 
//    System.out.println("["+ctx.getText()+"]");
//}
	@Override public void exitTableName(CreateTableParser.TableNameContext ctx) { 
        System.out.println("sssssssssssssssss");
        System.out.println(ctx.getText());
    }

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
//      listener.printDBInfo();
    }
};
