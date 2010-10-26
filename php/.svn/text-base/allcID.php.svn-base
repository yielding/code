#!/usr/local/php4/bin/php -q
<?
   function getJobCode($job) {
       $code_table = array (
          "전문직" => "005",
          "회사원" => "008",
          "자영업" => "998",
          "기타"   => "999",
          "주부"   => "997",
          "학생"   => "021"
       );
       $code = $code_table[$job];
       return $code;
   }

   $db = mysql_connect("www.tinc.co.kr", "webmaster", "web1234");

   mysql_select_db("tincdb001", $db);

   $sql = "select * from home_user";
   $res = mysql_query($sql, $db);
   $num = mysql_num_rows($res);

   for ($i=0; $i<$num; $i++) {
       $job_code = trim(getJobCode(mysql_result($res, $i, "job")));
       if ($job_code =="") continue;
       $name = mysql_result($res, $i, "name");
       $id   = mysql_result($res, $i, "id");
       print "update sys_svc_users set class='user' where svcid='$id'\n";
   }
?>
