using System.Collections;
using System.Collections.Generic;
using UnityEngine;


namespace PlaneSolver
{
    public class PlaneSolverPointGrab : MonoBehaviour
    {

        public Transform point1, point2, point3;

        public PresentPlane presentPlane;
        public PtManager ptManager;
        public PtOutputMenu ptOutputMenu;

        public ConstraintGrabbable pt1Grabber, pt2Grabber, pt3Grabber;
        public bool FixedPlane = false;

        public FlexButtonLockPlane lockButton;


        void Start()
        {
            // pt1Grabber.lastLocalPos = point1.localPosition;
            // grabbingPoint(point1, pt1Grabber);
            // pt2Grabber.lastLocalPos = point2.localPosition;
            // grabbingPoint(point2, pt2Grabber);
            // pt3Grabber.lastLocalPos = point3.localPosition;
            // grabbingPoint(point3, pt3Grabber);
        }
        void Update()
        {
            if (!pt1Grabber.IsGrabbed) pt1Grabber.lastLocalPos = point1.localPosition;
            else grabbingPoint(point1, pt1Grabber);
            if (!pt2Grabber.IsGrabbed) pt2Grabber.lastLocalPos = point2.localPosition;
            else grabbingPoint(point2, pt2Grabber);
            if (!pt3Grabber.IsGrabbed) pt3Grabber.lastLocalPos = point3.localPosition;
            else grabbingPoint(point3, pt3Grabber);
        }

        private void grabbingPoint(Transform point, ConstraintGrabbable grabber)
        {
            Vector3 newLoc = Vector3.zero;
            if (FixedPlane && presentPlane.forwardPlane.GetComponent<MeshRenderer>().enabled)
            {
                newLoc = Vector3.ProjectOnPlane(grabber.lastLocalPos - presentPlane.centerPt.localPosition, presentPlane.lookAtTarget.localPosition - presentPlane.plane.localPosition);
                newLoc = newLoc + presentPlane.centerPt.localPosition;
                if (newLoc.x > 10 || newLoc.x < -10 || newLoc.y > 10 || newLoc.y < -10 || newLoc.z > 10 || newLoc.z < -10)
                {
                    grabber.transform.position = point.position;
                }
                point.localPosition = newLoc;
            }
            else
            {
                FixedPlane = false;
                lockButton.LockOff();
                newLoc = grabber.lastLocalPos;
                point.localPosition = newLoc;
            }
            ptManager.updatePoint(point.name, presentPlane.UnscaledPoint(newLoc), FixedPlane);
        }
    }
}
