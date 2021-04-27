
namespace AABase.Logic
{
    public interface IArc : ICurve
    {
        AaPoint3d Center { get; }
        double Radius { get; }
        double StartAngle { get; }
        double EndAngle { get; }
        // the normal vector to the plane of the arc, expressed as simple 3d coordinate
        AaPoint3d PlaneNormal { get; }
    }
}
