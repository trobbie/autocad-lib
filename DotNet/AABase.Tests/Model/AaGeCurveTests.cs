using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using OverlapResultSummary = AABase.Logic.AaGeCurve.OverlapResultSummary;

namespace AABase.Tests
{
  [TestClass]
    public class AaGeCurveTests
    {
        static readonly double a = 0.0001; // slight shift in points that are _outside_ tolerance (should be seen as different)
        static readonly double b = 0.0000001; // slight shift in points that are _within_ tolerance (should be seen as same)
        static void TestOverlaps(string descTest, AaGeCurve thisCurve, AaGeCurve otherCurve, OverlapResultSummary expected)
        {
            // Act
            OverlapResultSummary test = thisCurve.Overlaps(otherCurve);
            // Assert
            if (!test.Equals(expected))
                Assert.Fail($"Failed test: {descTest}.\nReturned {test.ToString()}.\nExpected {expected.ToString()}.");
        }

        [TestMethod]
        public void Exact_X_Same()
        { 
            TestOverlaps("Exact_X_Same",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_X_SameWithFuzzX()
        { 
            TestOverlaps("Exact_X_SameWithFuzzX",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10+b,0,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_X_SameWithFuzzY()
        { 
            TestOverlaps("Exact_X_SameWithFuzzY",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,b,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void NoOverlap_X_AlmostX()
        { 
            TestOverlaps("NoOverlap_X_AlmostX",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10+a,a)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_X_AlmostY()
        { 
            TestOverlaps("NoOverlap_X_AlmostY",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,a,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_X_TouchingLines()
        { 
            TestOverlaps("NoOverlap_X_TouchingLines",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(10,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.NoOverlap);
        }

        [TestMethod]
        public void NoOverlap_X_Fuzz_Gap()
        { 
            TestOverlaps("NoOverlap_X_Fuzz_Gap",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(10+a,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void EndOverlaps_X_Fuzz_Overlap()
        { 
            TestOverlaps("EndOverlaps_X_Fuzz_Overlap",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(10,0,0)),
                        new AaGeCurve(new AaPoint3d(10-a,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.EndOverlapsOtherEnd);
        }
        [TestMethod]
        public void ContainsOther_X_Fuzz1()
        { 
            TestOverlaps("ContainsOther_X_Fuzz1",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(20-a,0,0)),
                        OverlapResultSummary.ContainsOther);
        }
        [TestMethod]
        public void ContainsOther_X_Fuzz2()
        { 
            TestOverlaps("ContainsOther_X_Fuzz2",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        new AaGeCurve(new AaPoint3d(a,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.ContainsOther);
        }
        [TestMethod]
        public void ContainedByOther_X_Fuzz1()
        { 
            TestOverlaps("ContainedByOther_X_Fuzz1",
                        new AaGeCurve(new AaPoint3d(a,0,0), new AaPoint3d(20,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.ContainedByOther);
        }
        [TestMethod]
        public void ContainedByOther_X_Fuzz2()
        { 
            TestOverlaps("ContainedByOther_X_Fuzz2",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(20-a,0,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(20,0,0)),
                        OverlapResultSummary.ContainedByOther);
        }
        [TestMethod]
        public void Exact_Y_Same()
        { 
            TestOverlaps("Exact_Y_Same",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_Y_SameWithFuzzX()
        { 
            TestOverlaps("Exact_Y_SameWithFuzzX",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(b,10,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void Exact_Y_SameWithFuzzY()
        { 
            TestOverlaps("Exact_Y_SameWithFuzzY",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10+b,0)),
                        OverlapResultSummary.Equals);
        }
        [TestMethod]
        public void NoOverlap_Y_AlmostX()
        { 
            TestOverlaps("NoOverlap_Y_AlmostX",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(a,10,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_Y_AlmostY()
        { 
            TestOverlaps("NoOverlapY_AlmostY",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(a,10+a,0)),
                        OverlapResultSummary.NoOverlap);
        }
        [TestMethod]
        public void NoOverlap_Y_TouchingLines()
        { 
            TestOverlaps("NoOverlap_Y_TouchingLines",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        new AaGeCurve(new AaPoint3d(0,10,0), new AaPoint3d(0,20,0)),
                        OverlapResultSummary.NoOverlap);
        }

        [TestMethod]
        public void NoOverlap_Y_Fuzz_Gap()
        { 
            TestOverlaps("NoOverlap_Y_Fuzz_Gap",
                        new AaGeCurve(new AaPoint3d(0,0,0), new AaPoint3d(0,10,0)),
                        new AaGeCurve(new AaPoint3d(0,10+a,0), new AaPoint3d(0,20,0)),
                        OverlapResultSummary.NoOverlap);
        }
    }
    
}