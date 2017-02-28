PROGRAM MTV3D
!-- Row Major Order - INCOMPLETE
!-- (Doesn't make sense in FORTRAN; this order is applicable for C)
!-- mpif90 mtv3d_sendrecv_type_int.f90; mpirun -n 1 ./a.out 

      USE MPI

      IMPLICIT NONE
      
      INTEGER ::    M=4, N=6, O=3
      INTEGER ::    S=3, T=4, U=2
      INTEGER, DIMENSION(:,:,:),  ALLOCATABLE :: A, B

      INTEGER :: TYPE_INT_XY, TYPE_INT_3D, SIZEOFINT, STATUS(MPI_STATUS_SIZE), IERR
      INTEGER :: RANK, NUMPROCS, DEST

      INTEGER :: K, J, I

      ALLOCATE  ( A (M,N,O) ) !M        N           O
      ALLOCATE  ( B (S,T,U) ) !S: rows, T: columns, U: slices / Depth
                              !I        J           K  iter vars
                              !X        Y           Z  Direction

!--   Initalize Arrays. A: Original Matrix. B: Sub-matrix

      DO K=1,O
         DO J=1,N
            DO I=1,M
               A(I,J,K) = I + J*10 + K*100
            ENDDO
         ENDDO
      ENDDO
      B = 99

!--   Fortran: Column Major order
!     All elements along first index (K) are contiguous in memory.
!     A(:,:,2) = 211  212  213  214  215  216
!                221  222  223  224  225  226
!                231  232  233  234  235  236
!                241  242  243  244  245  246

!     Print A linearly to check memory arrangement
!DBG  PRINT *, A  
!DBG  PRINT *, A(1,1,:)

      DO K = 1,O
         PRINT 1000, ((A(I,J,K), J=1,N), I=1,M)  ! Cache access not efficient
                                                 ! Order used only for print
      END DO
1000  FORMAT(' A=', /, 4(6(1X,I4.0), /))

!--   MPI Section

      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERR) 
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NUMPROCS, IERR) 

!--   SENDRECV as MPI_INTEGER - Row Major
!     CALL MPI_SENDRECV (A(2:S+1,2:T+1,2:U+1), S*T*U, MPI_INTEGER, 0, 111,  &
!                        B(1:S  ,1:T  ,1:U),   S*T*U, MPI_INTEGER, 0, 111,  &
!                        MPI_COMM_WORLD, STATUS, IERR)

!--------------------------------------------------------------------
!--   MPI Derived Data Type
!--------------------------------------------------------------------

!--   Get size of MPI_INTEGER
      CALL MPI_TYPE_EXTENT(MPI_INTEGER, SIZEOFINT, IERR)

      ! count       T = 2 blocks (columns)
      ! blocklength S = 3 rows in each column
      ! Stride      M = 6 elements in one column in original matrix

      CALL MPI_TYPE_HVECTOR( T, U,  O*SIZEOFINT,   MPI_INTEGER, TYPE_INT_XY, IERR)
      CALL MPI_TYPE_HVECTOR( S, 1,  O*N*SIZEOFINT, TYPE_INT_XY, TYPE_INT_3D, IERR)
      
      CALL MPI_TYPE_COMMIT(TYPE_INT_3D, IERR)

!--   Extract sub matrix: from 2nd to 4th row and 2nd to 5th column; Depth: 2nd to 3rd 


!--   SEND as MPI_TYPE_VECTOR; RECV as MPI_INTEGER
!     Enough to specify the first element of A that needs to be sent.
!     Count of A is 1; of TYPE_INT
 
      CALL MPI_SENDRECV (A(2,2,2), 1,     TYPE_INT_3D, 0, 111,  &
                         B(1,1,1), S*T*U, MPI_INTEGER, 0, 111,  &
                         MPI_COMM_WORLD, STATUS, IERR)
 

!--   Debug check: First index; contiuguos elements
!DBG  PRINT *,B

      DO K = 1,U
         PRINT 1001, ((B(I,J,K),I=1,S), J=1,T)
      END DO
1001  FORMAT(' B=', /, 3(4(1X,I4.0), /))

      DEALLOCATE (A, B)
!     CALL MPI_TYPE_FREE(TYPE_INT, IERR)
      CALL MPI_FINALIZE(IERR)

END
