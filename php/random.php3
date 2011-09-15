<?

function uniqueRandom(&$result, $range, $count) 
{
  if ($range < $count) return -1;

  $index = array();                           // 중복을 체크하기 위한 임시 배열
  for ($i=0; $i<$range; $i++) $index[$i] = 1;    // 임시 배열 초기화 

  $i = 0;                                     // 유일한 랜덤넘버 개수
  while ($i < $count) {
    $value = rand(0, $range);
    if ($index[$value] == 1) {               // 유일한 랜덤 넘버 발견
      $index[$value] = 0;
      $i++;
      $result[$i] = $value;
    }
  }
}

$problems = array (
  "leech", "dariopo", "spacedye", "yundang", "minsk", "yom", "ddong", "jo", "boss", "kamin"
);      

$index = array();
uniqueRandom($index, 10, 5);

for ($i=0; $i<10; $i++) {
  $v = $problems[$index[$i]];
  echo "$v <br>";
}
?>
~                                                                                                 
