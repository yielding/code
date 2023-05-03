package
{
	import flash.display.Sprite;
	import flash.events.Event;
	import flash.geom.Point;
	
	public class Light extends Sprite
	{
		private var clips_array:Array;
		private var points_array:Array;
		
		private var num_punti:int=100;
		
		public function Light()
		{
			init();
			initPoints();
			initDrawingListener();
		}
		
		private function init():void
		{
			stage.frameRate=31;
			
			clips_array=new Array();
			points_array=new Array();
		}
		
		private function initPoints():void
		{
			for(var i:int=0;i < num_punti;i++)
			{
				var xx:int=Math.random()*stage.stageWidth;
				var yy:int=Math.random()*stage.stageHeight;
				var point:Point=new Point(xx,yy);
				points_array.push(point);
				
				var sprite:Sprite=new Sprite();
				addChild(sprite);

				clips_array.push(sprite);
			}
		}
		
		private function initDrawingListener():void
		{
			addEventListener(Event.ENTER_FRAME, drawing);
		}
		
		private function drawing(e:Event):void
		{
			for(var i:int=0;i < clips_array.length;i++)
			{
				var xx:int=Math.random()*stage.stageWidth;
				var yy:int=Math.random()*stage.stageHeight;
				
				points_array[i].x=xx;
				points_array[i].y=yy;
				
				clips_array[i].graphics.clear();
				clips_array[i].graphics.moveTo(stage.stageWidth/2,stage.stageHeight/2);
				clips_array[i].graphics.lineStyle(.25,0xFFFFFF,.2);
				clips_array[i].graphics.lineTo(points_array[i].x,points_array[i].y);
			}
		}
	}
}


