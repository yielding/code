namespace OpenClosedPrinciple
{
    public class Spec<T>
    {
        public virtual bool IsSatisfied(T item) { return false; }

        public static AndSpec<T> operator & (Spec<T> o1, Spec<T> o2)
        {
            return new AndSpec<T>(o1, o2);
        }

        public static OrSpec<T> operator | (Spec<T> o1, Spec<T> o2)
        {
            return new OrSpec<T>(o1, o2);
        }
    }

    public class AndSpec<T> : Spec<T>
    {
        private Spec<T> first;
        private Spec<T> second;

        public AndSpec(Spec<T> first, Spec<T> second)
        {
            this.first = first;
            this.second = second;
        }

        public override bool IsSatisfied(T item)
        {
            return first.IsSatisfied(item) && second.IsSatisfied(item);
        }
    }

    public class OrSpec<T> : Spec<T>
    {
        private Spec<T> first;
        private Spec<T> second;

        public OrSpec(Spec<T> first, Spec<T> second)
        {
            this.first = first;
            this.second = second;
        }

        public override bool IsSatisfied(T item)
        {
            return first.IsSatisfied(item) || second.IsSatisfied(item);
        }
    }

    public class ColorSpec : Spec<Product>
    {
        private Color color;

        public ColorSpec(Color color)
        {
            this.color = color;
        }

        public override bool IsSatisfied(Product item)
        {
            return item.color == color;
        }
    }

    public class SizeSpec : Spec<Product>
    {
        private Size size;

        public SizeSpec(Size size)
        {
            this.size = size;
        }

        public override bool IsSatisfied(Product item)
        {
            return item.size == size;
        }
    }
}
