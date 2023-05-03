#!/usr/local/php4/bin/php -q

<?
   srand((double)microtime() * 1000000);

   /*
   $db = mysql_connect("www.tinc.co.kr", "webmaster", "web1234");
   mysql_select_db("tincdb001", $db);

   $sql = "select * from home_user order by passwd desc";
   $res = mysql_query($sql, $db);
   $num = mysql_num_rows($res);
   */

   for ($i=0; $i<5; $i++) {
       $index = rand(0, 340);
       //$name  = mysql_result($res, $index, "name");
       //$svcid = mysql_result($res, $index, "svcid");
       $name  = '이창하';
       $svcid = 'dirt72';
       $dd_   = rand(0, 30);
       $dd    = sprintf("%02d", $dd_);
       $i0    = rand(210, 219);
       $i1    = rand(2, 250);
       $i2    = rand(3, 250);
       $i3    = rand(4, 250);

       $sql = "insert into sys_log (lno, ldt, ctxmode, svcid, remote, item, cerror)
               values (sysloglno.nextval, 
                      to_date('2000-08-$dd', 'yyyy-mm-dd'),
                      'create', 
                      '$svcid', 
                      '$i1.$i2.$i3.$i4', 
                      'TOP',
                      0)";
   }

   for ($i=0; $i<644; $i++) {
       $index = rand(0, 643);
       //$name  = mysql_result($res, $index, "name");
       //$svcid = mysql_result($res, $index, "svcid");
       $name  = '이창하';
       $svcid = 'dirt72';
       $dd_   = rand(0, 30);
       $dd    = sprintf("%02d", $dd_);
       $i0    = rand(210, 219);
       $i1    = rand(2, 250);
       $i2    = rand(3, 250);
       $i3    = rand(4, 250);

       $sql = "insert into sys_log (lno, ldt, ctxmode, svcid, remote, item, cerror)
               values (sysloglno.nextval, 
                       to_date('2000-08-$dd', 'yyyy-mm-dd'),
                       'create', 
                       '$svcid', 
                       '$i1.$i2.$i3.$i4', 
                       'TOP',
                       0)";

   }

   for ($i=0; $i<1002; $i++) {
       $index = rand(0, 1002);
       //$name  = mysql_result($res, $index, "name");
       //$svcid = mysql_result($res, $index, "svcid");
       $name  = '이창하';
       $svcid = 'dirt72';
       $dd_   = rand(0, 30);
       $dd    = sprintf("%02d", $dd_);
       $i0    = rand(210, 219);
       $i1    = rand(2, 250);
       $i2    = rand(3, 250);
       $i3    = rand(4, 250);

       $sql = "insert into sys_log (lno, ldt, ctxmode, svcid, remote, item, cerror)
               values (sysloglno.nextval, 
                       to_date('2000-08-$dd', 'yyyy-mm-dd'),
                       'create', 
                       '$svcid', 
                       '$i1.$i2.$i3.$i4', 
                       'TOP',
                       0)";

   }
?>
