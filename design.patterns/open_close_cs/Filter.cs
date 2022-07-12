using System.Collections.Generic;
using System.Linq;

namespace OpenClosedPrinciple
{
    public class Filter_<T>
    {
        public virtual List<T> Filter(List<T> items, Spec<T> spec)
        {
            return new List<T>();
        }
    }

    public class BetterFilter : Filter_<Product>
    {
        public override List<Product> Filter(List<Product> items, Spec<Product> spec)
        {
            return items.FindAll(item => spec.IsSatisfied(item));
        }
    }
}
