using System;
using System.Linq;

namespace AABase.Logic
{
    public class AaPoint3d : IEquatable<AaPoint3d>, IComparable<AaPoint3d>
    {
        public AaPoint3d(AaPoint3d pt) { X = pt.X; Y = pt.Y; Z = pt.Z; }
        public AaPoint3d(double x, double y, double z) { X = x; Y = y; Z = z; }
        public AaPoint3d(double x, double y) { X = x; Y = y; Z = 0; }

        public AaPoint3d(double[] arrayDouble) { 
            if ((arrayDouble.Count() < 2) || (arrayDouble.Count() > 3))
                throw new ArgumentException();
            X = arrayDouble[0]; 
            Y = arrayDouble[1]; 
            if (arrayDouble.Count() == 3) Z = arrayDouble[2]; 
        }

        public static AaPoint3d operator+ (AaPoint3d a, AaPoint3d b)
        {
            return new AaPoint3d(a.X+b.X, a.Y+b.Y, a.Z+b.Z);
        }
        public static AaPoint3d operator- (AaPoint3d a, AaPoint3d b)
        {
            return new AaPoint3d(a.X-b.X, a.Y-b.Y, a.Z-b.Z);
        }

        public double X { get; private set; }
        public double Y { get; private set; }
        public double Z { get; private set; }

        public override bool Equals(object obj) => Equals(obj as AaPoint3d);
        public bool Equals(AaPoint3d otherPt)
        {
            return this.X.IsEqualTo(otherPt.X) && this.Y.IsEqualTo(otherPt.Y) && this.Z.IsEqualTo(otherPt.Z);
        }

        public override int GetHashCode()
        {
            int hash = 7;
            hash = 71 * hash + Math.Round(this.X, AABaseLogicGlobal.MaxPointPrecision).GetHashCode();
            hash = 71 * hash + Math.Round(this.Y, AABaseLogicGlobal.MaxPointPrecision).GetHashCode();
            hash = 71 * hash + Math.Round(this.Z, AABaseLogicGlobal.MaxPointPrecision).GetHashCode();
            return hash;
        }

        public int CompareTo(AaPoint3d other)
        {
            if (other == null) return 1;
            int result = this.X.CompareTo(other.X);
            return (result != 0) ? result : this.Y.CompareTo(other.Y);
        }

        public double[] AsArray() {
            double[] a = { X, Y, Z };
            return a;
        }

        public override string ToString()
        {
            if (Z == 0)
                return $"({X:F5}, {Y:F5})"; // more compact form (0 implied)
            else
                return $"({X:F5}, {Y:F5}, {Z:F5})";
        }
        
    }
}
