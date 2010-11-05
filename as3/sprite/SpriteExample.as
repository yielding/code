package
{
  import flash.display.Sprite;
  import flash.events.*;
  
  public class SpriteExample extends Sprite 
  {
    private var size:uint = 100;
    private var bgColor:uint = 0xFFCC00;
    private var box:Sprite;
    
    public function SpriteExample() 
    {
      var child:Sprite = new Sprite();
      child.addEventListener(MouseEvent.MOUSE_DOWN, mouseDownHandler);
      child.addEventListener(MouseEvent.MOUSE_UP, mouseUpHandler);
      draw(child);
      box = Sprite(addChild(child));
    }
    
    private function mouseDownHandler(event:MouseEvent):void 
    {
      trace("mouseDownHandler");
      var sprite:Sprite = Sprite(event.target);
      sprite.addEventListener(MouseEvent.MOUSE_MOVE, mouseMoveHandler);
      sprite.startDrag();
    }
    
    private function mouseUpHandler(event:MouseEvent):void 
    {
      trace("mouseUpHandler");
      var sprite:Sprite = Sprite(event.target);
      sprite.removeEventListener(MouseEvent.MOUSE_MOVE, mouseMoveHandler);
      sprite.stopDrag();
    }
    
    private function mouseMoveHandler(event:MouseEvent):void 
    {
      trace("mouseMoveHandler");
      event.updateAfterEvent();
    }
    
    private function draw(sprite:Sprite):void 
    {
      sprite.graphics.beginFill(bgColor);
      sprite.graphics.drawRect(0, 0, size, size);
      sprite.graphics.endFill();
    }
  }
}

