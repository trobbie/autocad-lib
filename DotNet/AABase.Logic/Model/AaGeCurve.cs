using System;

namespace AABase.Logic
{
    /// <summary>
    /// Basic geometry curve.  A straight curve is a straight line.
    /// </summary>
    /// <remarks>
    /// This assumes curve is on XY-plane, i.e. curve's plane normal is (0,0,1) or (0,0,-1).
    /// A (0,0,1) plane normal vector would produce a counter-clockwise arc.
    /// A (0,0,-1) plane normal vector would produce a clockwise arc.
    /// </remarks>
    public class AaGeCurve
    {
        /// <summary>
        /// Is the curve an arc.  If not, it is a line.
        /// </summary>
        /// <value></value>
        bool IsArc { get; }

        private readonly AaPoint3d _pt1;
        private readonly AaPoint3d _pt2;

        /// <summary>
        /// The center point of curve.
        /// </summary>
        AaPoint3d Center { get; }

        /// <summary>
        /// The radius of curve.
        /// </summary>
        double Radius { get; }
        
        /// <summary>
        /// The start angle, in radians, (on unit circle) of the curve within the curve's plane
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise.
        /// </remarks>
        double StartAngle { get; }
        /// <summary>
        /// The end angle, in radians, (on unit circle) of the curve within the curve's plane.
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise.
        /// </remarks>
        double EndAngle { get; }
        
        public AaGeCurve(AaPoint3d pt1, AaPoint3d pt2)
        {
            IsArc = false;
            Center = null;
            Radius = 0;
            StartAngle = 0;
            EndAngle = 0;
            _pt1 = pt1;
            _pt2 = pt2;
        }
        
        public AaGeCurve(AaPoint3d center, double radius, double startAngle, double endAngle, bool isClockwise)
        {
            IsArc = true;
            Center = center;
            Radius = radius;
            StartAngle = isClockwise ? endAngle : startAngle;
            EndAngle = isClockwise ? startAngle : endAngle;
            _pt1 = null;
            _pt2 = null;
        }

        public AaPoint3d StartPoint { get {
            return !IsArc ? _pt1 : 
                Center + new AaPoint3d(Radius * Math.Cos(StartAngle),
                                          Radius * Math.Sin(StartAngle),
                                          0);
        }}

        public AaPoint3d EndPoint { get {
            return !IsArc ? _pt2 : 
                Center + new AaPoint3d(Radius * Math.Cos(EndAngle),
                                          Radius * Math.Sin(EndAngle),
                                          0);
        }}

        public AaGeCurve GetCurveOrdered() {
            if (IsArc)
            {
                // arc points are already ordered since flipping Start and End produces a different arc
                return this;  
            }
            else
            {
                // an ordered line ensures that pt1's X is less than pt2's X, and if equal that the Y value is less
                if(_pt1.X.IsEqualTo(_pt2.X) || _pt1.X > _pt2.X)
                {
                    if (_pt1.Y.IsEqualTo(_pt2.Y) || _pt1.Y < _pt2.Y)
                    {
                        return this;
                    } 
                    else
                    {
                        return new AaGeCurve(_pt2, _pt1);
                    }
                } else
                {
                    return this;
                }
            }
        }

        public override bool Equals(object obj) => Equals(obj as AaGeCurve);
        public override int GetHashCode()
        {
            return IsArc ? Center.GetHashCode() ^ Radius.GetHashCode() ^ StartAngle.GetHashCode() ^ EndAngle.GetHashCode()
                         : _pt1.GetHashCode() ^ _pt2.GetHashCode();
        }
        public bool Equals(AaGeCurve curve)
        {
            return !(curve is null )
                && (IsArc || _pt1.Equals(curve._pt1))
                && (IsArc || _pt2.Equals(curve._pt2))
                && (!IsArc || Center.Equals(curve.Center))
                && (!IsArc || Radius.Equals(curve.Radius))
                && (!IsArc || StartAngle.Equals(curve.StartAngle))
                && (!IsArc || EndAngle.Equals(curve.EndAngle));
        }

        public int CompareTo(AaGeCurve other)
        {
            // If other is not a valid object reference, this instance is greater.
            if (other == null) return 1;
            AaGeCurve orderedCurve1 = this.GetCurveOrdered();
            AaGeCurve orderedCurve2 = other.GetCurveOrdered();
            // if one is line and other is arc, put line first
            if (IsArc != other.IsArc) return other.IsArc ? 1 : 0;
            if (IsArc)
            {
                int result = orderedCurve1.Center.CompareTo(orderedCurve2.Center);
                if (result != 0) return result;
                result = orderedCurve1.Radius.CompareTo(orderedCurve1.Radius);
                if (result != 0) return result;
                result = orderedCurve1.StartAngle.CompareTo(orderedCurve1.StartAngle);
                if (result != 0) return result;
                result = orderedCurve1.EndAngle.CompareTo(orderedCurve1.EndAngle);
                return result;
            } else
            {
                int result = orderedCurve1.StartPoint.CompareTo(orderedCurve2.StartPoint);
                 if (result != 0) return result;
                 return orderedCurve1.EndPoint.CompareTo(orderedCurve1.EndPoint);
            }
        }
        
        public override string ToString()
        {
            return $"[{_pt1.ToString()},{_pt2.ToString()}]";
        }

    }
}