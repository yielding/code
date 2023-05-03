#!/usr/bin/env php

<?
include("adodb/adodb.inc.php");

$conn = &ADONewConnection("mysql");
$conn->PConnect("localhost", "root", "", "test");
$rs = $conn->Execute("select * from memo");
while ($array = $rs->FetchRow()) {
  print_r($array);
}
$rs->Close();
?>
