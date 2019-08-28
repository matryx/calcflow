using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace Determinants
{
	public class CreateLine : MonoBehaviour {
		public Transform Point1;
		public Transform Point2;
		public Transform Point3;
		public Transform origin;
		public GameObject Line1;
		public GameObject Line2;
		public GameObject Line3;
		public GameObject Line4;
		public GameObject Line5;
		public GameObject Line6;
		public GameObject Line7;
		public GameObject Line8;
		public GameObject Line9;
		public GameObject Line10;
		public GameObject Line11;
		public GameObject Line12;

		// Update is called once per frame
		void Update () {
			//Get the positions of all the points
			var position0 = origin.localPosition;
			var position1 = Point1.localPosition;
			var position2 = Point2.localPosition;
			var position3 = Point3.localPosition;
			var position4 = position1+position2;
			var position5 = position1+position3;
			var position6 = position2+position3;
			var position7 = position1+position2+position3;

			// Create borders
			drawLine(position0,position1,Line1);
			drawLine(position0,position2,Line2);
			drawLine(position0,position3,Line3);
			drawLine(position1,position4,Line4);
			drawLine(position1,position5,Line5);
			drawLine(position2,position4,Line6);
			drawLine(position2,position6,Line7);
			drawLine(position3,position5,Line8);
			drawLine(position3,position6,Line9);
			drawLine(position4,position7,Line10);
			drawLine(position5,position7,Line11);
			drawLine(position6,position7,Line12);
		}

		void drawLine(Vector3 startV, Vector3 endV, GameObject cylinder){
			// Position
			cylinder.transform.localPosition = (endV + startV)/2.0F;

			// Rotation
			cylinder.transform.localRotation = Quaternion.FromToRotation(Vector3.up, endV-startV);

			// Scale        
			float dist = Vector3.Distance(endV, startV);

			cylinder.transform.localScale = new Vector3(0.1F, dist/2, 0.1F);
		}
	}
}
