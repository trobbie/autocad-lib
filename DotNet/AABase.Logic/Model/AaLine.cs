
using System;

namespace AABase.Logic
{
    public class AaLine : AaGeCurve
    {
        protected AaLine(AaPoint3d pt1, AaPoint3d pt2) : base(pt1, pt2)
        {
            // this constructor automatically sets the GeCurge to be a non-arc (line)
        }

    }
}