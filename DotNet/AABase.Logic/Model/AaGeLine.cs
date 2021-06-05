
using System;

namespace AABase.Logic
{
    internal class AaGeLine : AaGeCurve
    {
        internal AaGeLine(AaPoint3d pt1, AaPoint3d pt2) : base(pt1, pt2)
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
        public override bool IsEqualTo(IGeCurve curve, bool ignorePointOrder)
        {
            // TODO: move these first two checks to parent class
            if (curve is null) return false;
            if (!IsArc.Equals(curve.IsArc)) return false;
            
            return (StartPoint.Equals(curve.StartPoint) && EndPoint.Equals(curve.EndPoint))
                    || (ignorePointOrder && _pt1.Equals(curve.EndPoint) && _pt2.Equals(curve.StartPoint));
        }

    }
}