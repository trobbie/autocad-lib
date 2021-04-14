
namespace AABase.Logic
{
    public interface IArc : ICurve
    {
        AaPoint3d Center { get; }
        double Radius { get; }
    }
}
