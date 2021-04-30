using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using OverlapResult = AABase.Logic.AaGeCurve.OverlapResult;

namespace AABase.Tests
{
  [TestClass]
    public class AaGeCurveTests
    {
    
        static void TestOverlaps(string descTest, AaGeCurve thisCurve, AaGeCurve otherCurve, OverlapResult expected)
        {
            // Act
            OverlapResult test = thisCurve.Overlaps(otherCurve);
            // Assert
            if (!test.Equals(expected))
                Assert.Fail($"Failed test: {descTest}.\nReturned {test.ToString()}.\nExpected {expected.ToString()}.");
        }

        [TestMethod]
        public void Overlaps_Test1()
        { 
            TestOverlaps("TouchingLines Exact",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(10,0,0), new AaPoint3d(20,0,0)),
                        OverlapResult.NoOverlap);
        }

        [TestMethod]
        public void Overlaps_Test2()
        { 
            TestOverlaps("TouchingLines Fuzz Gap",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(10.0001,0,0), new AaPoint3d(20,0,0)),
                        OverlapResult.NoOverlap);
        }
        [TestMethod]
        public void Overlaps_Test3()
        { 
            TestOverlaps("TouchingLines Fuzz Overlap",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(9.9999,0,0), new AaPoint3d(20,0,0)),
                        OverlapResult.EndOverlapsOtherEnd);
        }
    }
    
}