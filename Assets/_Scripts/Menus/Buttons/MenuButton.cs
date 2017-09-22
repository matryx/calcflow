using UnityEngine;
using System.Collections;
using NanoVRController;
using CalcFlowUI;


abstract public class MenuButton : MonoBehaviour
{
    protected Color defaultColor;
    //protected Color selectedColor = Color.white;
    private string selectedColorHex = "#769FFF00";
    protected Color selectedColor;
    protected Color disabledColor = Color.gray;

    protected bool validCollision = false;

    public virtual void Awake()
    {
        if (!ColorUtility.TryParseHtmlString(selectedColorHex, out selectedColor))
            selectedColor = Color.white;

        defaultColor = GetComponent<Renderer>().material.color;
    }

    public virtual void OnTriggerEnter(Collider other)
    {
        // Only collide with the top face of the button (and only menu triggers)
        if (!other.CompareTag("MenuTrigger") || !IsTopFaceCollision(other))
        {
            validCollision = false;
            return;
        }

        TouchVRController controller = other.GetComponentInParent<TouchVRController>();
        if (controller != null)
            controller.SendHapticEvent(0.2f, 0.2f, 0.15f);

        validCollision = true;
        GetComponent<Renderer>().material.color = selectedColor;
    }

    public virtual void OnTriggerExit(Collider other)
    {
        GetComponent<Renderer>().material.color = defaultColor;
    }

    public virtual void OnTriggerStay(Collider other) { }

    protected bool IsTopFaceCollision(Collider other)
    {
        Vector3 direction = transform.position - other.transform.position;

        if (!(Vector3.Dot(transform.right, direction) < 0))
            return false;
        else
            return true;
    }
}
