namespace OpenClosedPrinciple
{
    public enum Color { Red, Green, Blue }

    public enum Size { Small, Medium, Large }

    public class Product
    {
        public string name;
        public Color color;
        public Size size;

        public Product(string name, Color color, Size size)
        {
            this.name  = name;
            this.color = color;
            this.size  = size;
        }

        public override string ToString()
        {
            return $"product: {name} / {color} / {size}";
        }
    }
}
