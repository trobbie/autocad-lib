using System;

namespace AABase.Logic
{
    public interface IExtents3d
    {
        Object GetObject();
        void AddExtents(IExtents3d source);

        AaPoint3d GetBottomLeft();
        AaPoint3d GetBottomRight();
        AaPoint3d GetTopLeft();
        AaPoint3d GetTopRight();

        double GetLeft(); // x
        double GetRight(); // x
        double GetTop(); // y
        double GetBottom(); // y

        double GetFront(); // z [looking down from (0,0,infinity)]

        double GetBack(); // z [looking down from (0,0,infinity)]

        AaPoint3d GetLeftCenterCenter();
        AaPoint3d GetRightCenterCenter();

        AaPoint3d GetCenterTopCenter();
        AaPoint3d GetCenterBottomCenter();

        double GetWidth(); // x

        double GetHeight(); // y

        double GetDepth(); // z

        Size3d GetSize();

        AaPoint3d GetCenter();

        double GetLargestDimensionValue();

        bool EnclosesExtents(IExtents3d extents);

        bool EnclosesPoint(AaPoint3d pt);
    }
}
