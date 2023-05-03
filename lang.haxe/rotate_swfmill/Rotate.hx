class Rotate
{
  static var mc: flash.MovieClip;

  static function main()
  {
    mc = flash.Lib._root.attachMovie("rails", "myclip", 0);
    if (mc == null)
      trace("myclip is not in the lib");

    mc._x = flash.Stage.width / 2;
    mc._y = flash.Stage.height / 2;
    mc.onEnterFrame = function()
    {
      mc._rotation += 3;
    }
  }

  // mc.onEnterFrame = loop;
  // static function loop()
  // {
  //   mc._rotation += 3;
  // }
}
