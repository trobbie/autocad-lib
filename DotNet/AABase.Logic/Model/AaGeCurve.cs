using System;
using System.Collections.Generic;

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
    public class AaGeCurve : IComparable<AaGeCurve> //, IEquatable<AaGeCurve>
    {
        /// <summary>
        /// Is the curve an arc.  If not, it is a line.
        /// </summary>
        /// <value></value>
        public bool IsArc { get; }

        private readonly AaPoint3d _pt1;
        private readonly AaPoint3d _pt2;

        /// <summary>
        /// The center point of curve.
        /// </summary>
        public AaPoint3d Center { get; }

        /// <summary>
        /// The radius of curve.
        /// </summary>
        public double Radius { get; }
        
        /// <summary>
        /// The start angle, in radians, (on unit circle) of the curve within the curve's plane
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise when viewed from it's plane's normal toward origin.
        /// </remarks>
        public double StartAngle { get; }
        /// <summary>
        /// The end angle, in radians, (on unit circle) of the curve within the curve's plane.
        /// </summary>
        /// <remarks>
        /// Arc is assumed to go counter-clockwise when viewed from it's plane's normal toward origin.
        /// </remarks>
        public double EndAngle { get; }

        /// <summary>
        /// The normal vector of the plane containing the curve
        /// </summary>
        public AaPoint3d PlaneNormal;

        public enum OverlapResult
        {
            NoOverlap = 0,
            Equals,
            ContainsOther,
            ContainedByOther,
            EndOverlapsOtherEnd
        }

        /// <summary>
        /// The slope of the curve, if a simple line. If an arc, return NaN
        /// </summary>
        public double Slope { get { return IsArc ? Double.NaN : (EndPoint.Y - StartPoint.Y) / (EndPoint.X - StartPoint.X); } }
        /// <summary>
        /// The y-intercept of the curve, if a simple line. If an arc, return NaN
        /// </summary>
        public double Yintercept { get { return IsArc ? Double.NaN : (EndPoint.Y - Slope * EndPoint.X); } }
        
        public AaGeCurve(AaPoint3d pt1, AaPoint3d pt2)
        {
            IsArc = false;
            Center = null;
            Radius = 0;
            StartAngle = 0;
            EndAngle = 0;
            PlaneNormal = null;
            _pt1 = pt1;
            _pt2 = pt2;
        }
        
        public AaGeCurve(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
        {
            IsArc = true;
            Center = center;
            Radius = radius;
            StartAngle = (planeNormal.Z < 0) ? endAngle : startAngle;
            EndAngle = (planeNormal.Z < 0) ? startAngle : endAngle;
            // since flipping angles if negative-Z normal, also adjust the new plane normal
            PlaneNormal = (planeNormal.Z >= 0) ? planeNormal : new AaPoint3d(-planeNormal.X, -planeNormal.Y, -planeNormal.Z);
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

        /*
        public override bool Equals(object obj) => Equals(obj as AaGeCurve);
        public override int GetHashCode()
        {
            return IsArc ? Center.GetHashCode() ^ Radius.GetHashCode() ^ StartAngle.GetHashCode() ^ EndAngle.GetHashCode()
                         : _pt1.GetHashCode() ^ _pt2.GetHashCode();
        }
        */
        public bool IsEqualTo(AaGeCurve curve)
        {
            return !(curve is null )
                && IsArc.Equals(curve.IsArc)
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
        
        public bool ContainsPoint(AaPoint3d pt) { 

            if (IsArc)
            {
                // TODO: calculate for arcs
                return false;
            }
            else
            {
                if (pt.Y.IsEqualTo(Slope*pt.X + Yintercept))
                {
                    // it is on "infinite" line, so now check if within domain
                    if (pt.X.IsEqualTo(StartPoint.X) || pt.X.IsEqualTo(EndPoint.X)) return true;
                    if (pt.X < StartPoint.X) return false;
                    if (pt.X > EndPoint.X) return false;
                    return true;
                }
                return false;
            }
        }

        public IEnumerable<AaGeCurve> FindOverlappingCurves(IEnumerable<AaGeCurve> listCurves)
        {
            List<AaGeCurve> overlappingCurves = new List<AaGeCurve>();
            foreach (AaGeCurve curve in listCurves)
            {
                // if overlaps this curve, then find the overlapping curve region and add to overlappingCurves
                // TODO: find overlapping area
                if (!this.Overlaps(curve).Equals(OverlapResult.NoOverlap))
                    overlappingCurves.Add(curve);
            }
            return overlappingCurves;
        }

        public OverlapResult Overlaps(AaGeCurve other)
        {
            if ((other is null) || !IsArc.Equals(other.IsArc)) return OverlapResult.NoOverlap;

            if (this.IsEqualTo(other)) return OverlapResult.Equals;

            if (IsArc)
            {
                // overlapping arcs must have same center, radius, and plane normal
                if (!this.Center.Equals(other.Center)) return OverlapResult.NoOverlap;
                if (!this.Radius.Equals(other.Radius)) return OverlapResult.NoOverlap;
                if (!this.PlaneNormal.Equals(other.PlaneNormal)) return OverlapResult.NoOverlap;
                
            }
            else // is simple line
            {
                // overlapping lines must have same slope
                if (!Slope.IsEqualTo(other.Slope)) return OverlapResult.NoOverlap;

                AaGeCurve thisOrdered = this.GetCurveOrdered();
                AaGeCurve otherOrdered = other.GetCurveOrdered();
                // curves' start/end points are now ordered; guarantee: start.x <= end.x, and if start.x=end.x, then start.y <= end.y

                // Note: at this moment, curves are guaranteed not equal
                if (thisOrdered.StartPoint.X.IsEqualTo(otherOrdered.StartPoint.X))
                {
                    if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                        return OverlapResult.ContainsOther;
                    else
                        return OverlapResult.ContainedByOther;
                }
                if (thisOrdered.StartPoint.X < otherOrdered.StartPoint.X)
                {
                    if (thisOrdered.ContainsPoint(otherOrdered.StartPoint))
                    {
                        if (thisOrdered.EndPoint.X.IsEqualTo(otherOrdered.StartPoint.X))
                            return OverlapResult.NoOverlap;
                        else if (thisOrdered.ContainsPoint(otherOrdered.EndPoint))
                            return OverlapResult.ContainsOther;
                        else
                            return OverlapResult.EndOverlapsOtherEnd;
                    }
                    else
                    {
                        return OverlapResult.NoOverlap;
                    }
                }
                // knowns: curves not equal,  this.start > other.start
                //if (thisOrdered.StartPoint.X > otherOrdered.StartPoint.X)
                {
                    if (otherOrdered.ContainsPoint(thisOrdered.StartPoint))
                    {
                        if (otherOrdered.EndPoint.X.IsEqualTo(thisOrdered.StartPoint.X))
                            return OverlapResult.NoOverlap;
                        else if (otherOrdered.ContainsPoint(thisOrdered.EndPoint))
                            return OverlapResult.ContainedByOther;
                        else
                            return OverlapResult.EndOverlapsOtherEnd;
                    }
                }
            }    


            return OverlapResult.NoOverlap;
        }

        public override string ToString()
        {
            return IsArc
                ? $"[Arc:{Center.ToString()},{Radius.ToString()},{StartAngle.ToString()},{EndAngle.ToString()}]"
                : $"[{_pt1.ToString()},{_pt2.ToString()}]";
        }

    }
}