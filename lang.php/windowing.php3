<?
   $scale      = 5; // 1페이지당 자료수
   $page_scale = 5; // 1화면당 페이지수
   $total      = 0; // 총 자료 개수
   $last       = 0; // 화면하단 [1] [2] .. 마지막번호 i.e. last window number
   $data       = array();   // 입력데이타
   $param      = "";

   //if (!$start) { $start = 0; }

   $sp_page = floor($start / ($scale * $page_scale)) ;

   $rc  = 50;             // 검색된 전체 레코드 수
   $num = $rc;    
   for ($i=0; $i<$num; $i++) $data[$i] = $i * 10;

   $total = $num;
   $last  = floor($total / $scale);  
   
   // what's n ?
   // $n = ($start) ? $total - $start : $total;

   // start 에서 scale 까지 만
   for ($i=$start; $i<$start + $scale; $i++) {
       //
       // 전체 자료 개수까지만 출력
       //
       if ($i < $total) {
          echo "\$n = $n \$i = $i <br>";
          // $tpl->assign();
          // $tpl->parse(); ok ?
      }
   }

   $url = "";
   //  검색 결과가 페이지 당 출력수 보다 크면 윈도우를 출력한다.
   if ($total > $scale) {
      if ($start+1 > $scale * $page_scale) {
         $pre_start = $start - ($scale * $page_scale);
         $url  = "$PHP_SELF?start=$pre_start";
         $line = "<a href=$url>이전</a>";
         echo $line;
      }

      for ($vj=0; $vj<$page_scale; $vj++) {
          $ln = ($sp_page * $page_scale + $vj) * $scale;
          $vk =  $sp_page * $page_scale + $vj + 1;

          if ($ln < $total) {
             if ($ln != $start) {
                $url  = "$PHP_SELF?start=$ln";
                $line = "[" . "<a href=$url>
                         <font size=2 color=black>$vk</font>
                         </a>" ."]";
                echo $line;
             } else{
                $line ="<font size=2 color=blue><b>$vk</b></font>";
                echo $line;
             }
          }
      }

      if ($total > (($sp_page + 1) * $scale * $page_scale)) { 
         $n_start = ($sp_page+1) * $scale * $page_scale ;
         $url     = "$PHP_SELF?start=$n_start";
         $line    = "<a href=$url>이후 </a>";
         echo $line;
      }
  }
?>
