
namespace AABase.Logic.Model
{
    public class AaLine
    {
        private readonly AaPoint3d _pt1;
        private readonly AaPoint3d _pt2;

        public AaLine(AaPoint3d pt1, AaPoint3d pt2)
        {
          _pt1 = pt1;
          _pt2 = pt2;
        }

        public bool Equals(AaLine line)
        {
            // check both points, either order
            return (_pt1.Equals(line._pt1) && _pt2.Equals(line._pt2))
                || (_pt1.Equals(line._pt2) && _pt2.Equals(line._pt1));
        }
        public override int GetHashCode()
        {
            return _pt1.GetHashCode() ^ _pt2.GetHashCode();
        }
    }
}