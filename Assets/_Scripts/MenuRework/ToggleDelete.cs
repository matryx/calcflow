using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false)]
public class ToggleDelete : QuickButton
{
    [SerializeField]
    GameObject deleteModeButton;
    [SerializeField]
    Transform confirmDeleteButton;
    float speed = 0.2f;
    Vector3 buttonScale;
    [SerializeField]
    Texture2D cancelIcon;
    [SerializeField]
    Texture2D deleteIcon;

    Renderer deleteModeRend;

    protected override void Start()
    {
        base.Start();
        deleteModeRend = deleteModeButton.GetComponent<Renderer>();
        buttonScale = confirmDeleteButton.localScale;
    }


    protected override void ButtonEnterBehavior(GameObject other)
    {
        if (!confirmDeleteButton.gameObject.activeSelf)
        {
            StartCoroutine(ActivateButton());
            deleteModeRend.material.mainTexture = cancelIcon;
        }
        else
        {
            StartCoroutine(DeactivateButton());
            deleteModeRend.material.mainTexture = deleteIcon;
        }
    }

    protected override void ButtonExitBehavior(GameObject other)
    {

    }

    IEnumerator DeactivateButton()
    {
        yield return StartCoroutine(ScaleTo(confirmDeleteButton, confirmDeleteButton.localScale, Vector3.zero, speed));
        confirmDeleteButton.gameObject.SetActive(false);
    }

    IEnumerator ActivateButton()
    {
        confirmDeleteButton.gameObject.SetActive(true);
        yield return StartCoroutine(ScaleTo(confirmDeleteButton, Vector3.zero, buttonScale, speed));
    }

    IEnumerator ScaleTo(Transform obj, Vector3 start, Vector3 end, float overTime)
    {
        float startTime = Time.time;

        while (Time.time < startTime + overTime)
        {
            obj.localScale = Vector3.Lerp(start, end, (Time.time - startTime) / overTime);
            yield return null;
        }

        obj.localScale = end;
    }

    void Update()
    {

    }
}
