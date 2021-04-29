
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
        
        public double Slope { get { return (EndPoint.Y - StartPoint.Y) / (EndPoint.X - StartPoint.X); } }
        public double Yintercept { get { return EndPoint.Y - Slope * EndPoint.X; } }

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
        public AaLine GetLineOrdered() {
            if(_pt1.X.IsEqualTo(_pt2.X) || _pt1.X > _pt2.X)
            {
                if (_pt1.Y.IsEqualTo(_pt2.Y) || _pt1.Y < _pt2.Y)
                {
                    return this;
                } 
                else
                {
                    return new AaLine(_pt2, _pt1);
                }
            } else
            {
                return this;
            }
        }
        public int CompareTo(AaLine other)
        {
            // If other is not a valid object reference, this instance is greater.
            if (other == null) return 1;
            AaLine orderedLine1 = this.GetLineOrdered();
            AaLine orderedLine2 = other.GetLineOrdered();
            int result = orderedLine1.StartPoint.CompareTo(orderedLine2.StartPoint);
            return (result != 0) ? result : orderedLine1.EndPoint.CompareTo(orderedLine1.EndPoint);
        }
        
        public bool ContainsPoint(AaPoint3d pt) { return pt.Y.IsEqualTo(Slope*pt.X + Yintercept); }
        
        public override string ToString()
        {
            return $"[{_pt1.ToString()},{_pt2.ToString()}]";
        }

    }
}