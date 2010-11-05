import flash.display.Sprite;
import flash.display.Shape;
import flash.events.Event;

class RailsClip extends Sprite {
  public function new() {
    super();
    flash.Lib.current.addChild(this);

    var circle:Shape = new Shape();
    circle.graphics.beginFill(0xff9933, 1);
    circle.graphics.drawCircle(0, 0, 40);
    circle.x = 40;
    circle.y = 40;

    addEventListener(Event.ENTER_FRAME, run);
    this.addChild(circle);
  }

  function run(event:Event):Void {
    this.x += 2;
    this.y += 1;
    if (this.x == 100) this.x = 10;
  }
}

class Rotate {
  static function main() {
    new RailsClip();
  }
}
