
using System;

namespace AABase.Logic
{
    public class AaLine : IEquatable<AaLine>, IComparable<AaLine>
    {
        private readonly AaPoint3d _pt1;
        private readonly AaPoint3d _pt2;

        public AaLine(AaPoint3d pt1, AaPoint3d pt2)
        {
          _pt1 = pt1;
          _pt2 = pt2;
        }

        public AaPoint3d StartPoint { get { return _pt1; }}
        public AaPoint3d EndPoint { get { return _pt2; }}
        
        public override bool Equals(object obj) => Equals(obj as AaLine);
        public override int GetHashCode()
        {
            return _pt1.GetHashCode() ^ _pt2.GetHashCode();
        }
        public bool Equals(AaLine line)
        {
            return line != null 
                && _pt1.Equals(line._pt1)
                && _pt2.Equals(line._pt2);
        }
        public bool EqualsIgnorePointOrder(AaLine line)
        {
            // check both points, either order
            return line != null 
                && ((_pt1.Equals(line._pt1) && _pt2.Equals(line._pt2))
                    || (_pt1.Equals(line._pt2) && _pt2.Equals(line._pt1)));
        }
        public int CompareTo(AaLine other)
        {
            // If other is not a valid object reference, this instance is greater.
            if (other == null) return 1;

            int result = _pt1.X.CompareTo(_pt2.X);
            return (result != 0) ? result : _pt1.Y.CompareTo(_pt2.Y);
        }
        
        public override string ToString()
        {
        return $"[{_pt1.ToString()},{_pt2.ToString()}]";
        }

    }
}