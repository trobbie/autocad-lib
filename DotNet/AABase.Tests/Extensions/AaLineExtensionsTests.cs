using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using System.Collections.Generic;
using System.Linq;

namespace AABase.Tests
{
  [TestClass]
    public class AaLineExtensionsTests
    {
        private static readonly List<FakeEntity> emptyList = new List<FakeEntity>();
        private static readonly List<FakeEntity> triangleLines1 = new List<FakeEntity>();
       [ClassInitialize]
        public static void InitializeFakeData(TestContext testContext) {
            // triangle (as individual lines)
            triangleLines1.Add(new FakeLineEntity(0, 0, 10, 0));
            triangleLines1.Add(new FakeLineEntity(10, 0, 10, 10));
            triangleLines1.Add(new FakeLineEntity(10, 10, 0, 0));
        }
        
        static void TestGetLineList(IEnumerable<IEntity>listObjects, List<AaLine> expected)
        {
            // Act
            IEnumerable<AaLine> test = listObjects.GetLineList();
            // Assert
            if (!test.OrderBy(t => t).SequenceEqual<AaLine>(expected.OrderBy(e => e)))
                Assert.Fail($"Failed args {listObjects}.\nReturned {test.ToStringDebug()}.\nExpected {expected.ToStringDebug()}.");
        }

        [TestMethod]
        public void GetLineList_EmptyList_ReturnEmptyList()
        { 
            List<AaLine> listLinesExpected = new List<AaLine>();

            TestGetLineList(emptyList, listLinesExpected);
        }

        [TestMethod]
        public void GetLineList_TEST1()
        { 
            List<AaLine> listLinesExpected = new List<AaLine>();
            listLinesExpected.Add(new AaLine(new AaPoint3d(0,0), new AaPoint3d(10,0)));
            listLinesExpected.Add(new AaLine(new AaPoint3d(10,0), new AaPoint3d(10,10)));
            listLinesExpected.Add(new AaLine(new AaPoint3d(10,10), new AaPoint3d(0,0)));

            TestGetLineList(triangleLines1, listLinesExpected);
        }

    }
    
}