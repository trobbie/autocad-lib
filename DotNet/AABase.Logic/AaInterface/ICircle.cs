
namespace AABase.Logic
{
    public interface ICircle : ICurve
    {
        AaPoint3d Center { get; }
        double Radius { get; }
        // the normal vector to the plane of the arc, expressed as simple 3d coordinate
        AaPoint3d PlaneNormal { get; }
    }
}
