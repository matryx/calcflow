using UnityEngine;
//using UnityEngine.UI;
using UnityEngine.EventSystems;

namespace Crosstales.UI
{
    /// <summary>Resize a UI element.</summary>
    public class UIResize : MonoBehaviour, IPointerDownHandler, IDragHandler
    {

        #region Variables

        /// <summary>Minimum size of the UI element.</summary>
        [Tooltip("Minimum size of the UI element.")]
        public Vector2 MinSize = new Vector2(300, 160);

        /// <summary>Maximum size of the UI element.</summary>
        [Tooltip("Maximum size of the UI element.")]
        public Vector2 MaxSize = new Vector2(800, 600);

        private RectTransform panelRectTransform;
        private Vector2 originalLocalPointerPosition;
        private Vector2 originalSizeDelta;
        private Vector2 originalSize;

        #endregion


        #region MonoBehaviour methods

        public void Awake()
        {
            panelRectTransform = transform.parent.GetComponent<RectTransform>();
            originalSize = new Vector2(panelRectTransform.rect.width, panelRectTransform.rect.height);
        }

        public void OnPointerDown(PointerEventData data)
        {
            originalSizeDelta = panelRectTransform.sizeDelta;

            RectTransformUtility.ScreenPointToLocalPointInRectangle(panelRectTransform, data.position, data.pressEventCamera, out originalLocalPointerPosition);
        }

        public void OnDrag(PointerEventData data)
        {
            if (panelRectTransform == null)
                return;

            Vector2 localPointerPosition;
            RectTransformUtility.ScreenPointToLocalPointInRectangle(panelRectTransform, data.position, data.pressEventCamera, out localPointerPosition);
            Vector3 offsetToOriginal = localPointerPosition - originalLocalPointerPosition;

            Vector2 sizeDelta = originalSizeDelta + new Vector2(offsetToOriginal.x, -offsetToOriginal.y);

            //Debug.Log("orig:" + originalSize + " - " + minSize.x);
            //Debug.Log("1:" + (originalSize.x + sizeDelta.x));

            if (originalSize.x + sizeDelta.x < MinSize.x)
            {
                sizeDelta.x = -(originalSize.x - MinSize.x);
            }
            else if (originalSize.x + sizeDelta.x > MaxSize.x)
            {
                sizeDelta.x = MaxSize.x - originalSize.x;
            }

            if (originalSize.y + sizeDelta.y < MinSize.y)
            {
                sizeDelta.y = -(originalSize.y - MinSize.y);
            }
            else if (originalSize.y + sizeDelta.y > MaxSize.y)
            {
                sizeDelta.y = MaxSize.y - originalSize.y;
            }

            /*
            sizeDelta = new Vector2 (
                Mathf.Clamp (sizeDelta.x, minSize.x, maxSize.x),
                Mathf.Clamp (sizeDelta.y, minSize.y, maxSize.y)
            );
            */

            //Debug.Log("2:" + sizeDelta);

            panelRectTransform.sizeDelta = sizeDelta;
        }

        #endregion
    }
}
// © 2018-2019 crosstales LLC (https://www.crosstales.com)