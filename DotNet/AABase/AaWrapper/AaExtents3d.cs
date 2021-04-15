using System;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Geometry;

namespace AABase.Logic
{
    public class AaExtents3d : IExtents3d
    {
        private Extents3d _extents;
        public AaExtents3d() { _extents = new Extents3d(); }

        public AaExtents3d(Extents3d extents) { _extents = extents; }

        public AaExtents3d(BoundBlock3d boundBlock) { _extents = Create(boundBlock.GetMinimumPoint().GetPoint(), boundBlock.GetMaximumPoint().GetPoint()); }

        public AaExtents3d(AaPoint3d minPoint, AaPoint3d maxPoint) { _extents = Create(minPoint, maxPoint); }

        private Extents3d Create(AaPoint3d minPoint, AaPoint3d maxPoint) 
        {
            Extents3d ext = new Extents3d();
            ext.AddPoint(minPoint.GetAutocadPoint3d());
            ext.AddPoint(maxPoint.GetAutocadPoint3d());
            return ext;
            //NOTE: the following was giving an Invalid Input message; not sure why
            //return new Extents3d(minPoint.GetAutocadPoint3d(), maxPoint.GetAutocadPoint3d());
        }

        public static IExtents3d Create() { return new AaExtents3d(new Extents3d()); }

        public AaPoint3d MinPoint { get => _extents.MinPoint.GetPoint(); }
        public AaPoint3d MaxPoint { get => _extents.MaxPoint.GetPoint(); }

        public void AddExtents(IExtents3d source)
        {
            _extents.AddExtents((Extents3d)source.GetObject());
        }

        public Object GetObject() { return _extents; }

        public AaPoint3d GetBottomLeft() {
            return new AaPoint3d(_extents.MinPoint.X, _extents.MinPoint.Y);
        }
        public AaPoint3d GetBottomRight()
        {
            return new AaPoint3d(_extents.MaxPoint.X, _extents.MinPoint.Y);
        }

        public AaPoint3d GetTopLeft()
        {
            return new AaPoint3d(_extents.MinPoint.X, _extents.MaxPoint.Y);
        }
        public AaPoint3d GetTopRight()
        {
            return new AaPoint3d(_extents.MaxPoint.X, _extents.MaxPoint.Y);
        }

        public double GetLeft()
        {
            return _extents.MinPoint.X;
        }
        public double GetRight()
        {
            return _extents.MaxPoint.X;
        }
        public double GetTop()
        {
            return _extents.MaxPoint.Y;
        }
        public double GetBottom()
        {
            return _extents.MinPoint.Y;
        }
        public double GetFront()
        {
            return _extents.MaxPoint.Z;
        }
        public double GetBack()
        {
            return _extents.MinPoint.Z;
        }

        public AaPoint3d GetLeftCenterCenter() {
            AaPoint3d center = GetCenter();
            return new AaPoint3d(GetLeft(), center.Y, center.Z);
        }
        public AaPoint3d GetRightCenterCenter()
        {
            AaPoint3d center = GetCenter();
            return new AaPoint3d(GetRight(), center.Y, center.Z);
        }

        public AaPoint3d GetCenterTopCenter()
        {
            AaPoint3d center = GetCenter();
            return new AaPoint3d(center.X, GetTop(), center.Z);
        }
        public AaPoint3d GetCenterBottomCenter()
        {
            AaPoint3d center = GetCenter();
            return new AaPoint3d(center.X, GetBottom(), center.Z);
        }

        public double GetWidth()
        {
            return GetRight() - GetLeft();
        }

        public double GetHeight()
        {
            return GetTop() - GetBottom();
        }

        public double GetDepth()
        {
            return GetFront() - GetBack();
        }

        public Size3d GetSize()
        {
            return _extents.GetSize();
        }
        public AaPoint3d GetCenter()
        {
            return new AaPoint3d((_extents.MinPoint.X + _extents.MaxPoint.X) / 2,
                               (_extents.MinPoint.Y + _extents.MaxPoint.Y) / 2,
                               (_extents.MinPoint.Z + _extents.MaxPoint.Z) / 2);
        }

        public double GetLargestDimensionValue()
        {
            return Math.Max(Math.Max(GetWidth(), GetHeight()), GetDepth());
        }

        public bool EnclosesExtents(IExtents3d extents)
        {
            return ((this.GetLeft() <= extents.GetLeft())
                    && (this.GetRight() >= extents.GetRight())
                    && (this.GetBottom() <= extents.GetBottom())
                    && (this.GetTop() >= extents.GetTop()));
        }

        public bool EnclosesPoint(AaPoint3d pt)
        {
            return ((this.GetLeft() <= pt.X)
                    && (this.GetRight() >= pt.X)
                    && (this.GetBottom() <= pt.Y)
                    && (this.GetTop() >= pt.Y)
                    && (this.GetBack() <= pt.Z)
                    && (this.GetFront() >= pt.Z));
        }

        public override string ToString()
        {
            return $"[BL: {GetBottomLeft().ToString()}, TR: {GetTopRight().ToString()}]";
        }
    }
}
