using System.Collections.Generic;

namespace OpenClosedPrinciple
{
    public class Filter<T>
    {
        public virtual List<T> filter(List<T> items, Specification<T> spec)
        {
            return new List<T>();
        }
    }

    public class BetterFilter : Filter<Product>
    {
        public override List<Product> filter(List<Product> items, Specification<Product> spec)
        {
            var result = new List<Product>();

            foreach (var p in items)
                if (spec.IsSatisfied(p))
                    result.Add(p);

            return result;
        }
    }
}
