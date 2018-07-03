using UnityEngine;
using System.Collections;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), false, false)]
public class LookAtText : ManualSerializeBehavior
{

    public GameObject cam;

    // Use this for initialization
    void Start()
    {
        if (FindObjectOfType<SteamVR_Camera>())
        {
            cam = FindObjectOfType<SteamVR_Camera>().gameObject;
        }
        else
        {
            cam = Camera.main.gameObject;
        }
    }

    // Update is called once per frame
    void Update()
    {
        if (!Replayer.Replaying)
        {
            transform.rotation = Quaternion.LookRotation(cam.transform.position - transform.position) * Quaternion.Euler(0, 180, 0);
        }
    }

    protected override void manualSerialize()
    {

    }
    protected override void manualDeserialize()
    {
        // print("this works!");
        // InitializeVariables();
        // InitializeParticleSystem();
        // restoreGradient();
    }
    public override void OnAfterRuntimeSerialize()
    {
        Destroy(this);
        this.gameObject.SetActive(false);
    }
}
