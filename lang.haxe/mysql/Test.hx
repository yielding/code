class Test {
  static function main() 
  {
    var conn = neko.db.Mysql.connect( {
      host: "localhost",
      port: 3306,
      user: "root",
      pass: "",
      socket: "/opt/local/var/run/mysql5/mysqld.sock",
      database: "openeye"
    });

    if (conn != null)
      trace("sjdkfsjdkfl");

    var rset = conn.request("select * from rfsignals");
    for (row in rset)
    {
      neko.Lib.print(row.t_id + ": " + row.r_id);
      trace (row.t_id);
    }

    conn.close();
  }
}
